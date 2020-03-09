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
import uk.gov.hmrc.jsonschema2scala.utils.TupleOps.PairExtensions

import scala.annotation.tailrec

object TypeDefinitionsBuilder {

  def buildFrom(schema: Schema)(implicit nameProvider: NameProvider): Either[List[String], TypeDefinition] = {

    val name = NameUtils.normalizedFirstUppercase(schema.name)

    val (types, context) =
      TypeDefinitionsBuilder
        .processSchema(name, Nil, schema, Context())
        .mapFirst(_.map { definition =>
          definition.copy(externalImports = TypeDefinitionsBuilder.calculateExternalImports(definition.schema))
        }.distinct)

    (if (types.isEmpty) Left(s"Schema ${schema.uri} is not a valid type definition" :: Nil)
     else if (types.size == 1) {
       Right(types.head)
     } else {
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
     })
      .map(processRemainingSchema(_, context))
  }

  def processSchema(name: String, path: List[String], schema: Schema, context: Context)(
    implicit nameProvider: NameProvider): (Seq[TypeDefinition], Context) =
    context.getTypeDefinition(schema.uriDecoded) match {
      case Some(Right(_)) => (Seq.empty, context)
      case other =>
        def withTemplates(body: Context => (Seq[TypeDefinition], Context)): (Seq[TypeDefinition], Context) = {
          val (templates, context2) = processTemplates(path, schema, context)
          body(context2).mapFirst(templates ++ _)
        }

        val (types: Seq[TypeDefinition], context2: Context) = schema match {
          case objectSchema: ObjectSchema =>
            processObjectSchema(name, path, objectSchema, context)

          case oneOfSchema: OneOfAnyOfSchema =>
            withTemplates(processOneOfAnyOfSchema(name, path, oneOfSchema, _))

          case allOfSchema: AllOfSchema =>
            withTemplates(processAllOfSchema(name, path, allOfSchema, _))

          case notSchema: NotSchema =>
            withTemplates(processSchema(name, path, notSchema.schema, _))

          case arraySchema: ArraySchema =>
            withTemplates(processArraySchema(name, path, arraySchema, _))

          case schemaReference: InternalSchemaReference =>
            processTemplates(
              path,
              schema,
              if (SchemaUtils.isPossiblyObjectSchema(schemaReference.schema)) {
                val schemas =
                  SchemaUtils.possibleObjectSchemas(schemaReference.schema)
                schemas.foldLeft(context)(_.addReference(_))
              } else context
            )

          case schemaStub: SchemaStub =>
            other match {
              case None => withTemplates(processSchema(name, path, schemaStub.resolved, _))
              case _    => (Seq.empty, context)
            }

          case _ =>
            processTemplates(path, schema, context)
        }

        val result = sortByName(avoidNameCollisions(types, ""))
        (result, context2.addTypeDefinitions(result))
    }

  def processTemplates(path: List[String], schema: Schema, context: Context)(
    implicit nameProvider: NameProvider): (Seq[TypeDefinition], Context) =
    traverse(schema.definitions, context) { (s, c) =>
      val childTypeName = nameProvider.toTypeName(s)
      processSchema(childTypeName, path, s, c)
    }

  def processObjectSchema(name: String, path: List[String], objectSchema: ObjectSchema, context: Context)(
    implicit nameProvider: NameProvider): (Seq[TypeDefinition], Context) =
    if (objectSchema.isEmpty) (Seq.empty, context)
    else {

      val typeName = {
        if (objectSchema.namedProperties.exists(_.name == name)) s"_$name" else name
      }

      val (templates, context2) = processTemplates(typeName :: path, objectSchema, context)

      val properties: Seq[Schema] = objectSchema.namedProperties ++
        objectSchema.patternProperties.getOrElse(Seq.empty) ++
        objectSchema.additionalProperties.map(Seq(_)).getOrElse(Seq.empty) ++
        objectSchema.unevaluatedProperties.map(Seq(_)).getOrElse(Seq.empty)

      val collectiveFieldsAggregated: Seq[CollectiveField] = aggregateCollectiveFields(objectSchema)

      if (path.nonEmpty && (objectSchema.isEmpty || objectSchema.hasSingleCollectiveFieldOnly)) {

        traverse(properties, context2) { (s, c) =>
          val childTypeName = nameProvider.toTypeName(s)
          processSchema(childTypeName, path, s, c)
        }

      } else {

        val (nestedTypeDefinitions, context3) =
          traverse(properties, context2) { (s, c) =>
            val childTypeName = nameProvider.toTypeName(s)
            processSchema(childTypeName, name :: path, s, c)
          }

        (
          Seq(
            TypeDefinition(
              name = typeName,
              path = path,
              schema = objectSchema,
              nestedTypes = sortByName(avoidNameCollisions(templates ++ nestedTypeDefinitions, typeName)),
              collectiveFieldsAggregated = collectiveFieldsAggregated
            )),
          context3)
      }
    }

  def processOneOfAnyOfSchema(name: String, path: List[String], oneOfSchema: OneOfAnyOfSchema, context: Context)(
    implicit nameProvider: NameProvider): (Seq[TypeDefinition], Context) = {

    val nonPrimitives: Seq[Schema] = oneOfSchema.variants.filterNot(_.primitive)
    val oneOfTypeName = nameProvider.toTypeName(oneOfSchema)

    val (arrays, nonArrays) = nonPrimitives.partition(SchemaUtils.isEffectiveArraySchema)

    var pos = 0

    val (arrayTypeDefinitions, context2) = traverse(arrays, context) { (s, c) =>
      val nameVariant =
        if (nonPrimitives.size == 1) name
        else {
          pos = pos + 1
          nameProvider.combine(name, s"_$pos")
        }
      processSchema(nameVariant, path, s, c)
    }

    val (nonArrayTypeDefinitions, context4) = nonArrays.size match {

      case 0 => (Seq.empty, context2)

      case 1 => processSchema(name, path, nonArrays.head, context2)

      case many =>
        val (directSubtypes, context3) = traverse(nonArrays, context2) { (s, c) =>
          val nameVariant = {
            pos = pos + 1
            nameProvider.combine(name, s"_$pos")
          }
          processSchema(nameVariant, name :: path, s, c)
        }.mapFirst(avoidNameCollisions(_, oneOfTypeName))

        val referencedSubtypesSchema: Seq[Schema] = nonArrays.flatMap {
          case internalReference: InternalSchemaReference =>
            context3.getTypeDefinition(internalReference.reference) match {

              case Some(Right(typeDef)) => Seq(typeDef.schema)

              case Some(Left(schema)) => Seq(schema)

              case None =>
                processSchema(
                  nameProvider.toTypeName(internalReference.schema),
                  Nil,
                  internalReference.schema,
                  context3)._1.map(_.schema)
            }

          case _ => Seq.empty
        }

        val subtypesSchemas = directSubtypes.map(_.schema) ++ referencedSubtypesSchema

        val types: Seq[TypeDefinition] = directSubtypes.size match {
          case 0 => Seq.empty
          case 1 =>
            Seq(
              TypeDefinition
                .modifyPath(safeTail)(directSubtypes.head)
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
              subtypes = subtypesSchemas,
              nestedTypes = directSubtypes
            )

            Seq(superType)
        }

        (types, context3)
    }

    (nonArrayTypeDefinitions ++ arrayTypeDefinitions, context4)
  }

  def processAllOfSchema(name: String, path: List[String], allOfSchema: AllOfSchema, context: Context)(
    implicit nameProvider: NameProvider): (Seq[TypeDefinition], Context) = {

    val (templates, context2) = traverse(allOfSchema.partials, context)((s, c) => processTemplates(name :: path, s, c))

    processSchema(name, path, allOfSchema.aggregatedSchema, context2)
      .mapFirst(templates ++ _)
  }

  def processArraySchema(name: String, path: List[String], arraySchema: ArraySchema, context: Context)(
    implicit nameProvider: NameProvider): (Seq[TypeDefinition], Context) =
    traverse(arraySchema.items.getOrElse(Seq.empty), context)((s, c) =>
      processSchema(nameProvider.toTypeName(s), path, s, c))

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

  def traverse(schemas: Seq[Schema], context: Context)(
    f: (Schema, Context) => (Seq[TypeDefinition], Context)): (Seq[TypeDefinition], Context) =
    schemas.foldLeft((Seq.empty[TypeDefinition], context)) {
      case ((definitions, c), schema) =>
        val (definitions2, c2) = f(schema, c)
        (definitions ++ definitions2, c2)
    }

  @tailrec
  def processRemainingSchema(typeDef: TypeDefinition, context: Context)(
    implicit nameProvider: NameProvider): TypeDefinition = {
    val unresolvedSchema: Seq[Schema] = context.remainingSchema
    if (unresolvedSchema.nonEmpty) {
      val (types2, context2) = traverse(unresolvedSchema, context) { (s, c) =>
        processSchema(nameProvider.toTypeName(s), typeDef.name :: typeDef.path, s, c)
      }
      val typeDef2 = typeDef.copy(nestedTypes = typeDef.nestedTypes ++ types2)
      if (types2.nonEmpty) processRemainingSchema(typeDef2, context2)
      else typeDef2
    } else typeDef
  }

  case class Context(private val definitions: Map[String, Either[Schema, TypeDefinition]] = Map.empty) {

    def getTypeDefinition(uri: String): Option[Either[Schema, TypeDefinition]] =
      definitions.get(uri)

    def addTypeDefinitions(typeDefs: Seq[TypeDefinition]): Context = {
      val definitions2 =
        typeDefs.foldLeft(definitions)((acc, typeDef) => acc.updated(typeDef.schema.uriDecoded, Right(typeDef)))
      this.copy(definitions = definitions2)
    }

    def addReference(schema: Schema): Context =
      if (definitions.get(schema.uriDecoded).flatMap(_.toOption).isDefined) this
      else this.copy(definitions = definitions.updated(schema.uriDecoded, Left(schema)))

    def remainingSchema: Seq[Schema] =
      definitions.collect { case (_, Left(schema)) => schema }.toSeq
  }
}
