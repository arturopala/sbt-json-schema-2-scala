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

package uk.gov.hmrc.jsonschema2scala.generator.scala2

import java.net.URI

import uk.gov.hmrc.jsonschema2scala.generator.TypeResolver
import uk.gov.hmrc.jsonschema2scala.schema._
import uk.gov.hmrc.jsonschema2scala.typer.TypeDefinition

import scala.collection.mutable

class ScalaTypeResolver(
  typeDef: TypeDefinition,
  buildTypeResolverFor: (Schema, Map[String, TypeResolver]) => TypeResolver,
  schemaReferenceResolver: SchemaReferenceResolver)
    extends TypeResolver {

  val schemaUriToTypePath: Map[String, List[String]] =
    TypeDefinition.listSchemaUriToTypePath(typeDef, excludeReferences = true).toMap

  lazy val schemaUriToTypeInterfaces: Map[String, Seq[Schema]] =
    TypeDefinition.listSchemaUriToTypeInterfaces(typeDef).groupBy(_._1).mapValues(_.flatMap(_._2))

  val externalTypeResolvers: mutable.Map[String, TypeResolver] =
    collection.mutable.Map[String, TypeResolver](
      typeDef.schema.uri -> this
    )

  def resolverForSchema(schema: Schema): TypeResolver =
    externalTypeResolvers.get(schema.uri) match {
      case Some(resolver) =>
        resolver

      case None =>
        val resolver = buildTypeResolverFor(schema, externalTypeResolvers.toMap)
        externalTypeResolvers.update(schema.uri, resolver)
        resolver
    }

  override val any: String = "Any"
  val anyRef: String = "AnyRef"

  println(s"Types resolved from ${schemaUriToTypePath.size} schema(s):")
  println(
    schemaUriToTypePath
      .mapValues(_.reverse.mkString("."))
      .toSeq
      .sortBy(_._1)
      .map { case (k, v) => s"\t$k -> $v" }
      .mkString("\n"))

  override def typeOf(schema: Schema, viewpoint: TypeDefinition, wrapAsOption: Boolean): String = {

    val typeName = schema match {

      case _: StringSchema  => "String"
      case _: NumberSchema  => "BigDecimal"
      case _: IntegerSchema => "Int"
      case _: BooleanSchema => "Boolean"
      case _: NullSchema    => any

      case objectSchema: ObjectSchema =>
        schemaTypeNameAsSeenFrom(objectSchema, viewpoint)
          .getOrElse(
            if (objectSchema.isEmpty) s"Map[String,$any]"
            else if (objectSchema.hasSingleCollectiveFieldOnly) {
              val fieldSchema = objectSchema.collectiveFields.head._2
              val schemaType = typeOf(fieldSchema, viewpoint, wrapAsOption = false)
              s"Map[String,$schemaType]"
            } else
              throw new IllegalStateException(
                s"Resolving type of object schema ${objectSchema.uri}, but the type definition unknown."))

      case arraySchema: ArraySchema =>
        arraySchema.items
          .map {
            case item :: Nil => s"Seq[${typeOf(item, viewpoint, wrapAsOption = false)}]"
            case many =>
              many.map(_.getClass.getSimpleName).distinct.toList match {
                case "JsObject" :: Nil => s"Seq[AnyRef]"
                case _ :: Nil          => s"Seq[${typeOf(many.head, viewpoint, wrapAsOption = false)}]"
                case _                 => s"Seq[$any]"
              }

          }
          .getOrElse(s"Seq[$any]")

      case oneOfSchema: OneOfAnyOfSchema => {
        val variants = oneOfSchema.variants.filter {
          case _: NullSchema => false
          case _             => true
        }
        if (oneOfSchema.variants.isEmpty) "Nothing"
        else if (variants.size == 1)
          typeOf(oneOfSchema.variants.head, viewpoint, wrapAsOption = false)
        else if (variants.forall(_.primitive)) {
          val types = variants
            .map(typeOf(_, viewpoint, wrapAsOption = false))
            .distinct
          if (types.size == 1) types.head else "AnyVal"
        } else if (variants.forall(v => !v.primitive)) {
          schemaTypeNameAsSeenFrom(oneOfSchema, viewpoint)
            .getOrElse(anyRef)
        } else "Any"
      }

      case allOfSchema: AllOfSchema =>
        typeOf(allOfSchema.aggregatedSchema, viewpoint, wrapAsOption = false)

      case not: NotSchema => typeOf(not.schema, viewpoint, wrapAsOption = false)

      case ite: IfThenElseSchema =>
        ite.elseSchema match {
          case None => typeOf(ite.schema, viewpoint, wrapAsOption = false)
          case Some(elseSchema) =>
            val schemaType = typeOf(ite.schema, viewpoint, wrapAsOption)
            val elseSchemaType = typeOf(elseSchema, viewpoint, wrapAsOption)
            if (schemaType == elseSchemaType) schemaType else anyRef
        }

      case internalReference: InternalSchemaReference =>
        typeOf(internalReference.schema, viewpoint, wrapAsOption = false)

      case externalReference: ExternalSchemaReference =>
        if (externalReference.schema.primitive) typeOf(externalReference.schema, viewpoint, wrapAsOption = false)
        else {
          externalReference.rootSchema
            .map(resolverForSchema)
            .getOrElse(this)
            .typeOf(externalReference.schema, viewpoint, wrapAsOption = false)
        }

      case schemaStub: SchemaStub =>
        val uri = URI.create(schemaStub.reference)
        schemaReferenceResolver
          .lookupSchema(uri, None)
          .map(_._1)
          .map(typeOf(_, viewpoint, wrapAsOption = false))
          .getOrElse(throw new IllegalStateException(
            s"Resolving type of schema stub reference ${schemaStub.reference}, but the type definition unknown."))

    }

    if (!schema.required && wrapAsOption) s"Option[$typeName]" else typeName
  }

  override def interfacesOf(schema: Schema, viewpoint: TypeDefinition): Set[String] =
    schemaUriToTypeInterfaces
      .get(schema.uri)
      .map(_.map { schema =>
        schemaTypeNameAsSeenFrom(schema, viewpoint)
          .getOrElse(throw new IllegalStateException(
            s"Unknown interface type for schema ${schema.uri} as seen from ${viewpoint.name}"))
      })
      .map(_.toSet)
      .getOrElse(Set.empty)

  def schemaTypeNameAsSeenFrom(schema: Schema, viewpoint: TypeDefinition): Option[String] =
    schemaTypeNameAsSeenFrom(schema.uri, viewpoint)

  def schemaTypeNameAsSeenFrom(uri: String, viewpoint: TypeDefinition): Option[String] =
    schemaUriToTypePath
      .get(uri)
      .map { targetPath =>
        val hostPath = viewpoint.path.reverse
        val guestPath = targetPath.reverse
        val relativePath = TypeResolver.shortenPrefix(guestPath, hostPath)
        relativePath.mkString(".")
      }

}
