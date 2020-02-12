/*
 * Copyright 2020 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.jsonschema2scala.typer

import uk.gov.hmrc.jsonschema2scala.schema._
import uk.gov.hmrc.jsonschema2scala.utils.NameUtils

object TypeDefinitionsBuilder {

  def buildFrom(schema: Schema)(implicit nameProvider: NameProvider): Either[List[String], TypeDefinition] = {

    val name = NameUtils.normalizedFirstUppercase(schema.name)
    val types = TypeDefinitionsBuilder
      .processSchema(name, Nil, schema)
      .map { definition =>
        definition.copy(externalImports = TypeDefinitionsBuilder.calculateExternalImports(definition.schema))
      }
      .distinct

    if (types.isEmpty) Left(s"Schema ${schema.uri} is not a valid type definition" :: Nil)
    else if (types.size == 1) Right(types.head)
    else {
      types.find(_.name == name) match {

        case Some(typeDef) => {

          val movingTypes: Seq[TypeDefinition] = types
            .filterNot(_ == typeDef)
            .map(TypeDefinition.modifyPath(prependNameIfMissing(typeDef.name)))

          Right(
            typeDef
              .copy(nestedTypes = typeDef.nestedTypes ++ movingTypes))
        }

        case None =>
          Right(
            TypeDefinition(
              name,
              Nil,
              ObjectSchema(
                attributes = SchemaAttributes(
                  name = schema.name,
                  path = schema.path,
                  description = schema.description,
                  required = schema.required,
                  custom = None)),
              nestedTypes = types
            ))
      }
    }
  }

  def processSchema(name: String, path: List[String], schema: Schema)(
    implicit nameProvider: NameProvider): Seq[TypeDefinition] = {

    lazy val templates: Seq[TypeDefinition] =
      processTemplates(path, schema)

    val types: Seq[TypeDefinition] = schema match {
      case objectSchema: ObjectSchema    => processObjectSchema(name, path, objectSchema)
      case oneOfSchema: OneOfAnyOfSchema => templates ++ processOneOfAnyOfSchema(name, path, oneOfSchema)
      case allOfSchema: AllOfSchema      => templates ++ processAllOfSchema(name, path, allOfSchema)
      case notSchema: NotSchema          => templates ++ processSchema(name, path, notSchema.schema)
      case arraySchema: ArraySchema      => templates ++ processArraySchema(name, path, arraySchema)
      case _                             => templates
    }

    sortByName(avoidNameCollisions(types, ""))
  }

  def processInternalSchemaReference(name: String, path: List[String], schema: Schema)(
    implicit nameProvider: NameProvider): Seq[TypeDefinition] = schema match {

    case internalReference: InternalSchemaReference =>
      processSchema(nameProvider.toTypeName(internalReference.schema), Nil, internalReference.schema)
        .map(_.copy(forReferenceOnly = true))

    case _ => Seq.empty
  }

  def processTemplates(path: List[String], schema: Schema)(implicit nameProvider: NameProvider): Seq[TypeDefinition] =
    schema.definitions
      .flatMap { schema =>
        val childTypeName = nameProvider.toTypeName(schema)
        processSchema(childTypeName, path, schema)
      }
      .sortBy(_.name)

  def processObjectSchema(name: String, path: List[String], objectSchema: ObjectSchema)(
    implicit nameProvider: NameProvider): Seq[TypeDefinition] =
    if (objectSchema.isEmpty) Seq.empty
    else {

      val typeName = {
        if (objectSchema.namedProperties.exists(_.name == name)) s"_$name" else name
      }

      val templates: Seq[TypeDefinition] = processTemplates(typeName :: path, objectSchema)

      val properties: Seq[Schema] = objectSchema.namedProperties ++
        objectSchema.patternProperties.getOrElse(Seq.empty) ++
        objectSchema.additionalProperties.map(Seq(_)).getOrElse(Seq.empty) ++
        objectSchema.unevaluatedProperties.map(Seq(_)).getOrElse(Seq.empty)

      val collectiveFieldsAggregated: Seq[CollectiveField] = aggregateCollectiveFields(objectSchema)

      if (path.nonEmpty && (objectSchema.isEmpty || objectSchema.hasSingleCollectiveFieldOnly)) {

        properties.flatMap { schema =>
          val childTypeName = nameProvider.toTypeName(schema)
          processSchema(childTypeName, path, schema)
        }

      } else {
        val nestedTypeDefinitions: Seq[TypeDefinition] =
          properties.flatMap { schema =>
            val childTypeName = nameProvider.toTypeName(schema)
            processSchema(childTypeName, name :: path, schema)
          }

        Seq(
          TypeDefinition(
            name = typeName,
            path = path,
            schema = objectSchema,
            nestedTypes = sortByName(avoidNameCollisions(templates ++ nestedTypeDefinitions, typeName)),
            collectiveFieldsAggregated = collectiveFieldsAggregated
          ))
      }
    }

  def processOneOfAnyOfSchema(name: String, path: List[String], oneOfSchema: OneOfAnyOfSchema)(
    implicit nameProvider: NameProvider): Seq[TypeDefinition] = {

    val nonPrimitives: Seq[Schema] = oneOfSchema.variants.filterNot(_.primitive)
    val oneOfTypeName = nameProvider.toTypeName(oneOfSchema)

    val (arrays, nonArrays) = nonPrimitives.partition(SchemaUtils.isEffectiveArraySchema)

    var pos = 0

    val arrayTypeDefinitions = arrays.flatMap { schema =>
      val nameVariant =
        if (nonPrimitives.size == 1) name
        else {
          pos = pos + 1
          nameProvider.combine(name, s"_$pos")
        }
      processSchema(nameVariant, path, schema) /*++
        processInternalSchemaReference(name, path, schema)*/
    }

    val nonArrayTypeDefinitions = nonArrays.size match {
      case 0 => Seq.empty
      case 1 =>
        processSchema(name, path, nonArrays.head)

      case many =>
        val directSubtypes = avoidNameCollisions(
          nonArrays.flatMap { schema =>
            val nameVariant = {
              pos = pos + 1
              nameProvider.combine(name, s"_$pos")
            }
            processSchema(nameVariant, name :: path, schema) /* ++
                  processInternalSchemaReference(name, name :: path, schema)*/
          },
          oneOfTypeName
        )

        val referencedSubtypes = nonArrays
          .flatMap { schema =>
            processInternalSchemaReference(name, name :: path, schema)
          }

        val subtypes = directSubtypes ++ referencedSubtypes

        subtypes.size match {
          case 0 => Seq.empty
          case 1 =>
            Seq(
              TypeDefinition
                .modifyPath(safeTail)(subtypes.head)
                .copy(name = name))

          case _ =>
            // New umbrella trait
            val superType = TypeDefinition(
              oneOfTypeName,
              path,
              ObjectSchema(
                attributes = SchemaAttributes(
                  name = name,
                  path = oneOfSchema.path,
                  description = oneOfSchema.description,
                  required = oneOfSchema.required,
                  custom = None)),
              isInterface = true,
              subtypes = subtypes.map(_.schema),
              nestedTypes = directSubtypes
            )

            Seq(superType)
        }
    }

    nonArrayTypeDefinitions ++ arrayTypeDefinitions
  }

  def processAllOfSchema(name: String, path: List[String], allOfSchema: AllOfSchema)(
    implicit nameProvider: NameProvider): Seq[TypeDefinition] = {

    val templates = allOfSchema.partials.flatMap(processTemplates(name :: path, _))

    templates ++ processSchema(name, path, allOfSchema.aggregatedSchema)
  }

  def processArraySchema(name: String, path: List[String], arraySchema: ArraySchema)(
    implicit nameProvider: NameProvider): Seq[TypeDefinition] =
    arraySchema.items.toSeq
      .flatMap(_.flatMap(item => processSchema(nameProvider.toTypeName(item), path, item)))

  def calculateExternalImports(schema: ObjectSchema)(implicit nameProvider: NameProvider): Set[String] =
    schema.properties.flatMap {
      case objectSchema: ObjectSchema =>
        calculateExternalImports(objectSchema)

      case oneOfAnyOfSchema: OneOfAnyOfSchema
          if oneOfAnyOfSchema.variants.collect { case _: ObjectSchema => }.nonEmpty =>
        oneOfAnyOfSchema.variants.collect { case o: ObjectSchema => o }.flatMap(calculateExternalImports)

      case allOfSchema: AllOfSchema if allOfSchema.partials.collect { case _: ObjectSchema => }.nonEmpty =>
        allOfSchema.partials.collect { case o: ObjectSchema => o }.flatMap(calculateExternalImports)

      case arraySchema: ArraySchema if arraySchema.items.exists(_.isInstanceOf[ExternalSchemaReference]) =>
        Set(nameProvider.toTypeName(arraySchema.items.asInstanceOf[ObjectSchema]))

      case NotSchema(_, objectSchema: ObjectSchema) => calculateExternalImports(objectSchema)

      case reference: ExternalSchemaReference if !reference.primitive =>
        Set(nameProvider.toTypeName(reference))

      case _ => Set()
    }.toSet

  def sortByName(definitions: Seq[TypeDefinition]): Seq[TypeDefinition] =
    definitions.sortBy(_.name)

  def safeTail[T](list: List[T]): List[T] = list match {
    case Nil     => Nil
    case _ :: xs => xs
  }

  def prependNameIfMissing(name: String): List[String] => List[String] = { path =>
    path.reverse match {
      case Nil     => name :: Nil
      case x :: xs => if (x == name) path else (name :: x :: xs).reverse
    }
  }

  def avoidNameCollisions(types: Seq[TypeDefinition], parentName: String)(
    implicit nameProvider: NameProvider): Seq[TypeDefinition] =
    if (types.size <= 1) types
    else {
      val names: Set[String] = (types.map(_.name) :+ parentName).toSet

      if (names.size == types.size + 1) types
      else
        types
          .foldLeft((Set(parentName), Seq.empty[TypeDefinition])) {
            case ((existingNames, modifiedTypes), typeDef) =>
              val typeName = typeDef.name
              val newTypeName = NameUtils.nextDistinctName(typeName, existingNames)
              (
                existingNames + newTypeName,
                modifiedTypes :+ (if (newTypeName == typeName) typeDef
                                  else TypeDefinition.changeName(newTypeName, typeDef)))
          }
          ._2

    }

  def aggregateCollectiveFields(objectSchema: ObjectSchema)(
    implicit nameProvider: NameProvider): Seq[CollectiveField] = {

    val ordinaryNames: Set[String] = objectSchema.namedProperties.map(_.name).toSet

    val fields: Map[String, (String, Schema)] = objectSchema.collectiveFields
      .foldLeft(Map.empty[String, (String, Schema)]) {

        case (fs, (id, schema)) =>
          val fieldName: String = SchemaUtils
            .referencedSchemaOption(schema)
            .map(s => s"${s.name.take(1).toLowerCase + s.name.drop(1)}Map")
            .getOrElse("properties")

          val updatedMap = fs.updated(
            id,
            (
              fieldName,
              SchemaUtils
                .referencedSchemaOption(schema)
                .getOrElse(schema)))

          updatedMap
      }
    // group fields with the same schema and ensure each field name is distinct
    fields
      .groupBy(_._2._2)
      .mapValues { vs =>
        val head = CollectiveField(vs.head._2._1, vs.head._2._2, Seq(vs.head._1))
        vs.tail.foldLeft(head) {
          case (CollectiveField(n, s, ids), (id, (_, _))) =>
            CollectiveField(n, s, ids :+ id)
        }
      }
      .values
      .foldLeft((ordinaryNames, Seq.empty[CollectiveField])) {
        case ((existingNames, modifiedFields), field) =>
          val modifiedName = nameProvider.toIdentifier(NameUtils.nextDistinctName(field.fieldName, existingNames))
          (existingNames + modifiedName, modifiedFields :+ field.copy(fieldName = modifiedName))
      }
      ._2
      .sortBy(_.fieldName)
  }
}
