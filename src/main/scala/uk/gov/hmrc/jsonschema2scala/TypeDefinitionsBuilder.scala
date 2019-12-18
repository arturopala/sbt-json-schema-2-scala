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

package uk.gov.hmrc.jsonschema2scala

import uk.gov.hmrc.jsonschema2scala.schema._

object TypeDefinitionsBuilder {

  def buildFrom(schema: Schema)(implicit typeNameProvider: TypeNameProvider): Either[List[String], TypeDefinition] = {
    val types = TypeDefinitionsBuilder
      .processSchema(schema.name, Nil, schema)
      .map { definition =>
        definition.copy(externalImports = TypeDefinitionsBuilder.calculateExternalImports(definition.schema))
      }
      .distinct

    types match {
      case Nil => Left(s"Schema ${schema.url} is not valid for type definition" :: Nil)
      case head :: tail =>
        tail match {
          case Nil => Right(head)
          case _   => Right(head.copy(nestedTypes = head.nestedTypes ++ tail))
        }
    }
  }

  def processSchema(name: String, path: List[String], schema: Schema)(
    implicit typeNameProvider: TypeNameProvider): Seq[TypeDefinition] = {

    lazy val templates = processTemplates(path.headOption.getOrElse(name), safeTail(path), schema)

    val types: Seq[TypeDefinition] = schema match {
      case objectSchema: ObjectSchema => processObjectSchema(name, path, objectSchema)
      case oneOfSchema: OneOfSchema   => processOneOfSchema(name, path, oneOfSchema) ++ templates
      case arraySchema: ArraySchema   => processArraySchema(name, path, arraySchema) ++ templates
      case mapSchema: MapSchema       => processMapSchema(name, path, mapSchema) ++ templates
      case _                          => templates
    }

    types
  }

  def processSchemaReferences(name: String, path: List[String], schema: Schema)(
    implicit typeNameProvider: TypeNameProvider): Seq[TypeDefinition] = schema match {

    case internalReference: InternalReference =>
      processSchema(typeNameProvider.toTypeName(internalReference.schema), Nil, internalReference.schema)
        .map(_.copy(forReferenceOnly = true))

    case externalReference: ExternalReference =>
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

    val templateNames: Set[String] = oneOfSchema.common.definitions.map(typeNameProvider.toTypeName).toSet

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
                  s"${typeNameProvider.toTypeName(schema)}_$pos"
                } else if (sameNameCounters.contains(subtypeName)) {
                  val counter = sameNameCounters(subtypeName)
                  val pos = counter.increment
                  s"${typeNameProvider.toTypeName(schema)}_$pos"
                } else typeNameProvider.toTypeName(schema)

              processSchema(subtypeNameVariant, name :: path, schema) ++ processSchemaReferences(
                subtypeNameVariant,
                name :: path,
                schema)
            }
          }
        // New interface type to span over multiple oneOf variants
        //val isRef = oneOf.isRef || subtypes.exists(_.schema.isRef)
        val superType = TypeDefinition(
          name,
          path,
          ObjectSchema(
            name = name,
            path = oneOfSchema.path,
            common = SchemaCommon(),
            properties = Seq.empty,
            required = Seq.empty,
            mandatory = oneOfSchema.mandatory,
            description = oneOfSchema.description
          ),
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

    val templateNames: Set[String] = mapSchema.common.definitions.map(typeNameProvider.toTypeName).toSet
    val eligible = mapSchema.properties.filterNot(_.isPrimitive)

    val typeDefinitions = eligible
      .flatMap { schema =>
        val childTypeName = {
          val n = typeNameProvider.toTypeName(schema)
          if (templateNames.contains(n)) s"${name}_$n" else n
        }
        processSchema(childTypeName, name :: path, schema)
      }

    sortByName(typeDefinitions)
  }

  def listTemplateNames(objectSchema: ObjectSchema, typeNameProvider: TypeNameProvider): Set[String] =
    objectSchema.common.definitions.map(typeNameProvider.toTypeName).toSet

  def calculateExternalImports(schema: ObjectSchema)(implicit typeNameProvider: TypeNameProvider): Set[String] =
    schema.properties.flatMap {
      case o: ObjectSchema => calculateExternalImports(o)
      case oneOf: OneOfSchema if oneOf.variants.collect { case _: ObjectSchema => }.nonEmpty =>
        oneOf.variants.collect { case o: ObjectSchema => o }.flatMap(calculateExternalImports)
      case a: ArraySchema if a.item.isInstanceOf[ExternalReference] =>
        Set(typeNameProvider.toTypeName(a.item.asInstanceOf[ObjectSchema]))
      case e: ExternalReference if !e.isPrimitive =>
        Set(typeNameProvider.toTypeName(e))
      case _ => Set()
    }.toSet

  def sortByName(definitions: Seq[TypeDefinition]): Seq[TypeDefinition] =
    definitions.sortBy(_.name)

  def safeTail[T](list: List[T]): List[T] = list match {
    case Nil     => Nil
    case _ :: xs => xs
  }

  case class Counter(initial: Int = 0) {
    @volatile
    var v: Int = initial
    def increment: Int = {
      v = v + 1
      v
    }
  }
}
