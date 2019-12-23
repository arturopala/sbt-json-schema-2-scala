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

  override def typeOf(
    schema: Schema,
    viewpoint: TypeDefinition,
    wrapAsOption: Boolean = true,
    showDefaultValue: Boolean = true): String = {

    val typeName = schema match {

      case _: StringSchema  => "String"
      case _: NumberSchema  => "BigDecimal"
      case _: IntegerSchema => "Int"
      case _: BooleanSchema => "Boolean"
      case _: NullSchema    => any

      case objectSchema: ObjectSchema => schemaTypeNameAsSeenFrom(objectSchema, viewpoint)

      case mapSchema: MapSchema =>
        if (mapSchema.patternProperties.size == 1)
          s"Map[String,${typeOf(mapSchema.patternProperties.head, viewpoint, wrapAsOption = false, showDefaultValue = false)}]"
        else s"Map[String,$any]"

      case arraySchema: ArraySchema =>
        arraySchema.items
          .map {
            case item :: Nil => s"Seq[${typeOf(item, viewpoint, wrapAsOption = false, showDefaultValue = false)}]"
            case many =>
              many.map(_.getClass.getSimpleName).distinct.toList match {
                case "JsObject" :: Nil => s"Seq[AnyRef]"
                case _ :: Nil          => s"Seq[${typeOf(many.head, viewpoint, wrapAsOption = false, showDefaultValue = false)}]"
                case _                 => s"Seq[$any]"
              }

          }
          .getOrElse(s"Seq[$any]")

      case oneOfSchema: OneOfAnyOfSchema =>
        if (oneOfSchema.variants.isEmpty) "Nothing"
        else if (oneOfSchema.variants.size == 1) typeOf(oneOfSchema.variants.head, viewpoint, wrapAsOption = false)
        else if (oneOfSchema.variants.forall(_.primitive)) {
          val types = oneOfSchema.variants
            .map(typeOf(_, viewpoint, wrapAsOption = false, showDefaultValue = false))
            .distinct
          if (types.size == 1) types.head else "AnyVal"
        } else if (oneOfSchema.variants.forall(v => !v.primitive)) schemaTypeNameAsSeenFrom(oneOfSchema, viewpoint)
        else "Any"

      case not: NotSchema => typeOf(not.schema, viewpoint, wrapAsOption = false, showDefaultValue)

      case ite: IfThenElseSchema =>
        ite.elseSchema match {
          case None => typeOf(ite.schema, viewpoint, wrapAsOption = false, showDefaultValue)
          case Some(elseSchema) =>
            val schemaType = typeOf(ite.schema, viewpoint, wrapAsOption, showDefaultValue)
            val elseSchemaType = typeOf(elseSchema, viewpoint, wrapAsOption, showDefaultValue)
            if (schemaType == elseSchemaType) schemaType else "AnyRef"
        }

      case internalReference: InternalSchemaReference =>
        typeOf(internalReference.schema, viewpoint, wrapAsOption = false, showDefaultValue = false)

      case externalReference: ExternalSchemaReference =>
        typeOf(externalReference.schema, viewpoint, wrapAsOption = false, showDefaultValue = false)

      case schemaStub: SchemaStub =>
        schemaTypeNameAsSeenFrom(schemaStub.reference, viewpoint)
    }

    if (!schema.required && wrapAsOption) s"Option[$typeName]${if (showDefaultValue) " = None" else ""}"
    else { s"""$typeName${if (showDefaultValue && schema.boolean) " = false" else ""}""" }
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

  def schemaTypeNameAsSeenFrom(schema: Schema, viewpoint: TypeDefinition): String =
    schemaTypeNameAsSeenFrom(schema.uri, viewpoint)

  def schemaTypeNameAsSeenFrom(uri: String, viewpoint: TypeDefinition): String = {
    val typeName = schemaUriToTypePath.get(uri) match {
      case Some(targetPath) =>
        val hostPath = viewpoint.path.reverse
        val guestPath = targetPath.reverse
        val relativePath = TypeResolver.shortenPrefix(guestPath, hostPath)
        relativePath.mkString(".")

      case None =>
        throw new IllegalStateException(s"Resolving type of schema $uri, but the type definition unknown.")
    }

    typeName
  }

}
