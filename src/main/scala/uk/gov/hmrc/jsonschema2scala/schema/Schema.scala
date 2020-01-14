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

package uk.gov.hmrc.jsonschema2scala.schema

import play.api.libs.json.{JsValue, Reads}

sealed trait Schema {

  final def name: String = attributes.name
  final def path: List[String] = attributes.path
  final def description: Option[String] = attributes.description
  final def definitions: Seq[Schema] = attributes.definitions

  final def custom[T: Reads](key: String): Option[T] =
    for {
      map    <- attributes.custom
      value  <- map.get(key)
      result <- value.asOpt[T]
    } yield result

  final def hasCustom(key: String): Boolean =
    attributes.custom.exists(_.contains(key))

  val attributes: SchemaAttributes

  val primitive: Boolean
  val validated: Boolean

  val boolean: Boolean = false
  def required: Boolean = attributes.required

  val uri: String = {
    val normalizedPath = if (path.nonEmpty && (path.head == "not")) path.tail else path
    SchemaReferenceResolver.pathToReference(normalizedPath)
  }

  def withDefinitions(definitions: Seq[Schema]): Schema =
    SchemaUtils.copyAttributes(this, attributes.copy(definitions = definitions))
}

case class SchemaAttributes(
  name: String,
  path: List[String],
  description: Option[String],
  definitions: Seq[Schema] = Seq.empty,
  required: Boolean,
  custom: Option[Map[String, JsValue]]
)

case class NullSchema(attributes: SchemaAttributes) extends Schema {
  override val primitive: Boolean = true
  override val validated: Boolean = false
  final override val required: Boolean = false
}

case class StringSchema(
  attributes: SchemaAttributes,
  pattern: Option[String] = None,
  minLength: Option[Int] = None,
  maxLength: Option[Int] = None,
  enum: Option[Seq[String]] = None)
    extends Schema {

  override val primitive: Boolean = true
  override val validated: Boolean =
    enum.isDefined || pattern.isDefined || minLength.isDefined || maxLength.isDefined
}

case class NumberSchema(
  attributes: SchemaAttributes,
  minimum: Option[BigDecimal] = None,
  maximum: Option[BigDecimal] = None,
  exclusiveMinimum: Option[BigDecimal] = None,
  exclusiveMaximum: Option[BigDecimal] = None,
  multipleOf: Option[BigDecimal] = None,
  enum: Option[Seq[BigDecimal]] = None
) extends Schema {

  override val primitive: Boolean = true
  override val validated: Boolean = minimum.isDefined || maximum.isDefined || multipleOf.isDefined
}

case class IntegerSchema(
  attributes: SchemaAttributes,
  minimum: Option[Int] = None,
  maximum: Option[Int] = None,
  exclusiveMinimum: Option[Int] = None,
  exclusiveMaximum: Option[Int] = None,
  multipleOf: Option[Int] = None,
  enum: Option[Seq[Int]] = None
) extends Schema {

  override val primitive: Boolean = true
  override val validated: Boolean = minimum.isDefined || maximum.isDefined || multipleOf.isDefined
}

case class BooleanSchema(attributes: SchemaAttributes, enum: Option[Seq[Boolean]] = None) extends Schema {
  override val primitive: Boolean = true
  override val boolean: Boolean = true
  override val validated: Boolean = false
  final override val required: Boolean = true
}

case class ArraySchema(
  attributes: SchemaAttributes,
  items: Option[Seq[Schema]] = None,
  minItems: Option[Int] = None,
  maxItems: Option[Int] = None,
  uniqueItems: Option[Boolean] = None,
  minContains: Option[Int] = None,
  maxContains: Option[Int] = None)
    extends Schema {

  override val primitive: Boolean = false
  override val validated: Boolean =
    items.exists(_.exists(_.validated)) || minItems.isDefined || maxItems.isDefined

  def allItemsPrimitive: Boolean = items.forall(_.forall(_.primitive))
}

case class ObjectSchema(
  attributes: SchemaAttributes,
  properties: Seq[Schema] = Seq.empty,
  requiredFields: Seq[String] = Seq.empty,
  alternativeRequiredFields: Seq[Set[String]] = Seq.empty,
  patternProperties: Option[Seq[Schema]] = None)
    extends Schema {

  final val primitive: Boolean = false
  final val validated: Boolean = true

  def isEmpty: Boolean = properties.isEmpty && patternProperties.isEmpty
}

case class MapSchema(
  attributes: SchemaAttributes,
  patternProperties: Seq[Schema] = Seq.empty,
  requiredFields: Seq[String] = Seq.empty,
  alternativeRequiredFields: Seq[Set[String]] = Seq.empty)
    extends Schema {

  override val primitive: Boolean = false
  override val validated: Boolean = true

  def isEmpty: Boolean = patternProperties.isEmpty
}

case class OneOfAnyOfSchema(
  attributes: SchemaAttributes,
  variants: Seq[Schema] = Seq.empty,
  alternativeRequiredFields: Seq[Set[String]] = Seq.empty,
  isOneOf: Boolean)
    extends Schema {

  override val primitive: Boolean = variants.forall(_.primitive)
  override val validated: Boolean = variants.nonEmpty
}

case class AllOfSchema(
  attributes: SchemaAttributes,
  variants: Seq[Schema] = Seq.empty,
  requiredFields: Seq[String] = Seq.empty,
  aggregatedSchema: Schema)
    extends Schema {

  override val primitive: Boolean = aggregatedSchema.primitive
  override val validated: Boolean = variants.nonEmpty
}

case class NotSchema(attributes: SchemaAttributes, schema: Schema) extends Schema {

  override val primitive: Boolean = schema.primitive
  override val validated: Boolean = true
}

case class IfThenElseSchema(attributes: SchemaAttributes, condition: Schema, schema: Schema, elseSchema: Option[Schema])
    extends Schema {

  override val primitive: Boolean = schema.primitive && elseSchema.exists(_.primitive)
  override val validated: Boolean = true
}

case class InternalSchemaReference(
  attributes: SchemaAttributes,
  reference: String,
  schema: Schema,
  requiredFields: Seq[String])
    extends Schema {

  override val primitive: Boolean = schema.primitive
  override val required: Boolean = requiredFields.contains(name)
  override val validated: Boolean = schema.validated
}

case class ExternalSchemaReference(
  attributes: SchemaAttributes,
  reference: String,
  schema: Schema,
  requiredFields: Seq[String])
    extends Schema {

  override val primitive: Boolean = schema.primitive
  override val required: Boolean = requiredFields.contains(name)
  override val validated: Boolean = schema.validated
}

case class SchemaStub(attributes: SchemaAttributes, reference: String) extends Schema {

  override val required: Boolean = false
  override val validated: Boolean = false
  override val primitive: Boolean = false

}

object SchemaStub {
  def apply(schema: Schema, reference: String): SchemaStub =
    new SchemaStub(schema.attributes, reference)
}
