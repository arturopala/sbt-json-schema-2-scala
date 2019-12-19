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
      case _: NullSchema    => "Any"

      case arraySchema: ArraySchema =>
        s"Seq[${typeOf(arraySchema.item, viewpoint, wrapAsOption = false, showDefaultValue = false)}]"

      case objectSchema: ObjectSchema => schemaTypeNameAsSeenFrom(objectSchema, viewpoint)

      case mapSchema: MapSchema =>
        if (mapSchema.patternProperties.size == 1)
          s"Map[String,${typeOf(mapSchema.patternProperties.head, viewpoint, wrapAsOption = false, showDefaultValue = false)}]"
        else "Map[String,Any]"

      case oneOfSchema: OneOfSchema =>
        if (oneOfSchema.variants.isEmpty) "Nothing"
        else if (oneOfSchema.variants.size == 1) typeOf(oneOfSchema.variants.head, viewpoint, wrapAsOption = false)
        else if (oneOfSchema.variants.forall(_.isPrimitive)) "AnyVal"
        else if (oneOfSchema.variants.forall(v => !v.isPrimitive)) schemaTypeNameAsSeenFrom(oneOfSchema, viewpoint)
        else "Any"

      case internalReference: InternalSchemaReference =>
        typeOf(internalReference.schema, viewpoint, wrapAsOption = false, showDefaultValue = false)

      case externalReference: ExternalSchemaReference =>
        typeOf(externalReference.schema, viewpoint, wrapAsOption = false, showDefaultValue = false)
    }

    if (!schema.mandatory && wrapAsOption) s"Option[$typeName]${if (showDefaultValue) " = None" else ""}"
    else { s"""$typeName${if (showDefaultValue && schema.isBoolean) " = false" else ""}""" }
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

  def schemaTypeNameAsSeenFrom(schema: Schema, viewpoint: TypeDefinition): String = {
    val typeName = schemaUriToTypePath.get(schema.uri) match {
      case Some(targetPath) =>
        val hostPath = viewpoint.path.reverse
        val guestPath = targetPath.reverse
        val relativePath = TypeResolver.shortenPrefix(guestPath, hostPath)
        relativePath.mkString(".")

      case None =>
        throw new IllegalStateException(s"Unexpected error, cannot find type definition for schema ${schema.uri}")
    }

    typeName
  }

}
