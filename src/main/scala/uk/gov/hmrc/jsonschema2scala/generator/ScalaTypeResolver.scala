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

package uk.gov.hmrc.jsonschema2scala.generator

import uk.gov.hmrc.jsonschema2scala.schema._
import uk.gov.hmrc.jsonschema2scala.typer.{TypeDefinition, TypeNameProvider}

class ScalaTypeResolver(
  schemaUriToTypePath: Map[String, List[String]],
  schemaUriToTypeInterfaces: Map[String, Seq[List[String]]])(implicit schemaNameResolver: TypeNameProvider)
    extends TypeResolver {

  override val any: String = "Any"

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
            throw new IllegalStateException(
              s"Resolving type of object schema ${objectSchema.uri}, but the type definition unknown."))

      case mapSchema: MapSchema =>
        if (mapSchema.patternProperties.size == 1)
          s"Map[String,${typeOf(mapSchema.patternProperties.head, viewpoint, wrapAsOption = false)}]"
        else s"Map[String,$any]"

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
            .getOrElse("AnyRef")
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
            if (schemaType == elseSchemaType) schemaType else "AnyRef"
        }

      case internalReference: InternalSchemaReference =>
        typeOf(internalReference.schema, viewpoint, wrapAsOption = false)

      case externalReference: ExternalSchemaReference =>
        typeOf(externalReference.schema, viewpoint, wrapAsOption = false)

      case schemaStub: SchemaStub =>
        schemaTypeNameAsSeenFrom(schemaStub.reference, viewpoint)
          .getOrElse(
            throw new IllegalStateException(
              s"Resolving type of schema reference ${schemaStub.reference}, but the type definition unknown."))

    }

    if (!schema.required && wrapAsOption) s"Option[$typeName]" else typeName
  }

  override def interfacesOf(schema: Schema, viewpoint: TypeDefinition): Set[String] = {
    val hostPath = viewpoint.path.reverse
    schemaUriToTypeInterfaces
      .get(schema.uri)
      .map(_.map { interfacePath =>
        val guestPath = interfacePath.reverse
        val relativePath = TypeResolver.shortenPrefix(guestPath, hostPath)
        relativePath.mkString(".")
      })
      .map(_.toSet)
      .getOrElse(Set.empty)
  }

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
