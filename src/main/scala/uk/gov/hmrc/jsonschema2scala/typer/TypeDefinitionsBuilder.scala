/*
 * Copyright 2019 HM Revenue & Customs
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

object TypeDefinitionsBuilder {

  def buildFrom(schema: Schema)(implicit typeNameProvider: TypeNameProvider): Either[List[String], TypeDefinition] = {
    val name = typeNameProvider.toTypeName(schema)
    val types = TypeDefinitionsBuilder
      .processSchema(name, Nil, schema)
      .map { definition =>
        definition.copy(externalImports = TypeDefinitionsBuilder.calculateExternalImports(definition.schema))
      }
      .distinct

    if (types.isEmpty) Left(s"Schema ${schema.uri} is not valid for type definition" :: Nil)
    else if (types.size == 1) Right(types.head)
    else {
      types.find(_.name == name) match {

        case Some(typeDef) => {
          val embeddedTypes: Seq[TypeDefinition] = types
            .filterNot(_ == typeDef)
            .map(TypeDefinition.modifyPath(prependNameIfMissing(typeDef.name)))

          Right(typeDef.copy(nestedTypes = typeDef.nestedTypes ++ embeddedTypes))
        }

        case None =>
          Right(
            TypeDefinition(
              name,
              Nil,
              ObjectSchema(
                name = schema.name,
                path = schema.path,
                description = schema.description,
                mandatory = schema.mandatory),
              nestedTypes = types
            ))
      }
    }
  }

  def processSchema(name: String, path: List[String], schema: Schema)(
    implicit typeNameProvider: TypeNameProvider): Seq[TypeDefinition] = {

    lazy val templates: Seq[TypeDefinition] =
      processTemplates(path.headOption.getOrElse(name), safeTail(path), schema)

    val types: Seq[TypeDefinition] = schema match {
      case objectSchema: ObjectSchema        => processObjectSchema(name, path, objectSchema)
      case oneOfSchema: OneOfSchema          => processOneOfSchema(name, path, oneOfSchema) ++ templates
      case arraySchema: ArraySchema          => processArraySchema(name, path, arraySchema) ++ templates
      case mapSchema: MapSchema              => processMapSchema(name, path, mapSchema) ++ templates
      case external: ExternalSchemaReference => processExternalSchemaReference(name, path, external) ++ templates
      case _                                 => templates
    }

    types
  }

  def processInternalSchemaReference(name: String, path: List[String], schema: Schema)(
    implicit typeNameProvider: TypeNameProvider): Seq[TypeDefinition] = schema match {

    case internalReference: InternalSchemaReference =>
      processSchema(typeNameProvider.toTypeName(internalReference.schema), Nil, internalReference.schema)
        .map(_.copy(forReferenceOnly = true))

    case _ => Seq.empty
  }

  def processExternalSchemaReference(name: String, path: List[String], schema: Schema)(
    implicit typeNameProvider: TypeNameProvider): Seq[TypeDefinition] = schema match {

    case externalReference: ExternalSchemaReference =>
      processSchema(typeNameProvider.toTypeName(externalReference.schema), Nil, externalReference.schema)
        .map(_.copy(forReferenceOnly = true))

    case _ => Seq.empty
  }

  def processTemplates(name: String, path: List[String], schema: Schema)(
    implicit typeNameProvider: TypeNameProvider): Seq[TypeDefinition] =
    schema.common.definitions
      .flatMap { schema =>
        val childTypeName = typeNameProvider.toTypeName(schema)
        processSchema(childTypeName, name :: path, schema)
      }
      .sortBy(_.name)

  def processObjectSchema(name: String, path: List[String], objectSchema: ObjectSchema)(
    implicit typeNameProvider: TypeNameProvider): Seq[TypeDefinition] = {

    val templates: Seq[TypeDefinition] = processTemplates(name, path, objectSchema)
    val templateNames: Set[String] = listTemplateNames(objectSchema, typeNameProvider)

    val nestedTypeDefinitions: Seq[TypeDefinition] =
      objectSchema.properties.flatMap { schema =>
        val childTypeName = {
          val n = typeNameProvider.toTypeName(schema)
          if (templateNames.contains(n)) s"${name}_$n" else n
        }
        processSchema(childTypeName, name :: path, schema)
      }

    Seq(TypeDefinition(name, path, objectSchema, sortByName(nestedTypeDefinitions ++ templates)))
  }

  def processOneOfSchema(name: String, path: List[String], oneOfSchema: OneOfSchema)(
    implicit typeNameProvider: TypeNameProvider): Seq[TypeDefinition] = {

    val templateNames: Set[String] = listTemplateNames(oneOfSchema, typeNameProvider)

    val eligible: Seq[Schema] = oneOfSchema.variants.filterNot(_.isPrimitive)

    val typeDefinitions = eligible.size match {

      case 0 => Seq.empty

      case 1 =>
        processSchema(name, path, eligible.head)

      case many =>
        val oneOfTypeName = typeNameProvider.toTypeName(oneOfSchema)
        val sameNameCounters: Map[String, Counter] =
          eligible
            .map(typeNameProvider.toTypeName)
            .groupBy(identity)
            .mapValues(_.size)
            .filter(_._2 > 1)
            .mapValues(_ => Counter())
            .toSeq
            .toMap
        var pos = 0
        val subtypes = eligible
          .flatMap { schema =>
            {
              val subtypeName = {
                val n = typeNameProvider.toTypeName(schema)
                if (templateNames.contains(n)) s"${name}_$n" else n
              }

              val subtypeNameVariant =
                if (typeNameProvider.toTypeName(schema) == oneOfTypeName) {
                  pos = pos + 1
                  typeNameProvider.toTypeNameVariant(schema, pos)
                } else if (sameNameCounters.contains(subtypeName)) {
                  val counter = sameNameCounters(subtypeName)
                  val pos = counter.increment
                  typeNameProvider.toTypeNameVariant(schema, pos)
                } else typeNameProvider.toTypeName(schema)

              processSchema(subtypeNameVariant, name :: path, schema) ++
                processInternalSchemaReference(subtypeNameVariant, name :: path, schema) ++
                processExternalSchemaReference(subtypeNameVariant, name :: path, schema)
            }
          }
        // New interface type to span over multiple oneOf variants
        //val isRef = oneOf.isRef || subtypes.exists(_.schema.isRef)
        val superType = TypeDefinition(
          oneOfTypeName,
          path,
          ObjectSchema(
            name = name,
            path = oneOfSchema.path,
            description = oneOfSchema.description,
            mandatory = oneOfSchema.mandatory),
          isInterface = true,
          subtypes = subtypes
        )
        val subtypesExtendingSuperType = subtypes.map(s => s.copy(interfaces = s.interfaces :+ superType))
        Seq(
          superType.copy(
            subtypes = subtypesExtendingSuperType,
            nestedTypes = subtypesExtendingSuperType.filterNot(_.forReferenceOnly)))
    }

    sortByName(typeDefinitions)
  }

  def processArraySchema(name: String, path: List[String], arraySchema: ArraySchema)(
    implicit typeNameProvider: TypeNameProvider): Seq[TypeDefinition] =
    processSchema(
      typeNameProvider.toTypeName(arraySchema.item),
      path,
      arraySchema.item
    )

  def processMapSchema(name: String, path: List[String], mapSchema: MapSchema)(
    implicit typeNameProvider: TypeNameProvider): Seq[TypeDefinition] = {

    val eligible = mapSchema.patternProperties.filterNot(_.isPrimitive)

    val typeDefinitions = eligible
      .flatMap { schema =>
        val childTypeName = typeNameProvider.toTypePatternName(schema)
        processSchema(childTypeName, name :: path, schema)
      }

    sortByName(typeDefinitions)
  }

  def listTemplateNames(schema: Schema, typeNameProvider: TypeNameProvider): Set[String] =
    schema.common.definitions.map(typeNameProvider.toTypeName).toSet

  def calculateExternalImports(schema: ObjectSchema)(implicit typeNameProvider: TypeNameProvider): Set[String] =
    schema.properties.flatMap {
      case o: ObjectSchema => calculateExternalImports(o)
      case oneOf: OneOfSchema if oneOf.variants.collect { case _: ObjectSchema => }.nonEmpty =>
        oneOf.variants.collect { case o: ObjectSchema => o }.flatMap(calculateExternalImports)
      case a: ArraySchema if a.item.isInstanceOf[ExternalSchemaReference] =>
        Set(typeNameProvider.toTypeName(a.item.asInstanceOf[ObjectSchema]))
      case e: ExternalSchemaReference if !e.isPrimitive =>
        Set(typeNameProvider.toTypeName(e))
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

  case class Counter(initial: Int = 0) {
    var v: Int = initial
    def increment: Int = {
      v = v + 1
      v
    }
  }
}
