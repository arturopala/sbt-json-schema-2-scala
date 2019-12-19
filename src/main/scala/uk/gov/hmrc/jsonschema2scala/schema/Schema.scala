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

sealed trait Schema {

  val name: String
  val path: List[String]
  val description: Option[String]

  val common: SchemaCommon

  def mandatory: Boolean
  def validate: Boolean

  val isPrimitive: Boolean = true
  val isBoolean: Boolean = false

  val uri: String = SchemaReferenceResolver.pathToUri(path)
}

case class SchemaCommon(definitions: Seq[Schema] = Seq.empty)

case class ObjectSchema(
  name: String,
  path: List[String],
  common: SchemaCommon = SchemaCommon(),
  description: Option[String] = None,
  mandatory: Boolean = false,
  properties: Seq[Schema] = Seq.empty,
  requiredFields: Seq[String] = Seq.empty,
  alternativeRequiredFields: Seq[Set[String]] = Seq.empty,
  patternProperties: Option[Seq[Schema]] = None)
    extends Schema {
  override val isPrimitive: Boolean = false
  override def validate: Boolean = true
  def isEmpty: Boolean = properties.isEmpty && patternProperties.isEmpty
}

case class MapSchema(
  name: String,
  path: List[String],
  common: SchemaCommon = SchemaCommon(),
  description: Option[String] = None,
  mandatory: Boolean = false,
  patternProperties: Seq[Schema] = Seq.empty,
  requiredFields: Seq[String] = Seq.empty,
  alternativeRequiredFields: Seq[Set[String]] = Seq.empty)
    extends Schema {
  override val isPrimitive: Boolean = false
  override def validate: Boolean = true
  def isEmpty: Boolean = patternProperties.isEmpty
}

case class OneOfSchema(
  name: String,
  path: List[String],
  common: SchemaCommon = SchemaCommon(),
  description: Option[String] = None,
  mandatory: Boolean = false,
  variants: Seq[Schema] = Seq.empty,
  alternativeRequiredFields: Seq[Set[String]] = Seq.empty)
    extends Schema {
  override def validate: Boolean = variants.nonEmpty
  override val isPrimitive: Boolean = variants.forall(_.isPrimitive)
}

case class StringSchema(
  name: String,
  path: List[String],
  common: SchemaCommon = SchemaCommon(),
  description: Option[String] = None,
  mandatory: Boolean = false,
  pattern: Option[String] = None,
  enum: Option[Seq[String]] = None,
  minLength: Option[Int] = None,
  maxLength: Option[Int] = None,
  isUniqueKey: Boolean = false,
  isKey: Boolean = false,
  customGenerator: Option[String] = None)
    extends Schema {
  override def validate: Boolean =
    enum.isDefined || pattern.isDefined || minLength.isDefined || maxLength.isDefined
}

case class NumberSchema(
  name: String,
  path: List[String],
  common: SchemaCommon = SchemaCommon(),
  description: Option[String] = None,
  mandatory: Boolean = false,
  customGenerator: Option[String] = None,
  minimum: Option[BigDecimal] = None,
  maximum: Option[BigDecimal] = None,
  multipleOf: Option[BigDecimal] = None
) extends Schema {
  override def validate: Boolean = minimum.isDefined || maximum.isDefined || multipleOf.isDefined
}

case class IntegerSchema(
  name: String,
  path: List[String],
  common: SchemaCommon = SchemaCommon(),
  description: Option[String] = None,
  mandatory: Boolean = false,
  customGenerator: Option[String] = None,
  minimum: Option[Int] = None,
  maximum: Option[Int] = None,
  multipleOf: Option[Int] = None
) extends Schema {
  override def validate: Boolean = minimum.isDefined || maximum.isDefined || multipleOf.isDefined
}

case class BooleanSchema(
  name: String,
  path: List[String],
  common: SchemaCommon,
  description: Option[String] = None,
  mandatory: Boolean)
    extends Schema {
  override val isBoolean: Boolean = true
  override def validate: Boolean = false
}

case class NullSchema(name: String, path: List[String], common: SchemaCommon, description: Option[String] = None)
    extends Schema {
  override def validate: Boolean = false
  override def mandatory: Boolean = false
}

case class ArraySchema(
  name: String,
  path: List[String],
  common: SchemaCommon,
  description: Option[String] = None,
  mandatory: Boolean,
  item: Schema,
  minItems: Option[Int] = None,
  maxItems: Option[Int] = None)
    extends Schema {
  override val isPrimitive: Boolean = false
  override def validate: Boolean = item.validate || minItems.isDefined || maxItems.isDefined
}

case class InternalSchemaReference(
  name: String,
  path: List[String],
  common: SchemaCommon,
  description: Option[String] = None,
  reference: String,
  schema: Schema,
  requiredFields: Seq[String])
    extends Schema {
  override val isPrimitive: Boolean = schema.isPrimitive
  override def mandatory: Boolean = requiredFields.contains(name)
  override def validate: Boolean = schema.validate
}

case class ExternalSchemaReference(
  name: String,
  path: List[String],
  common: SchemaCommon,
  description: Option[String] = None,
  reference: String,
  schema: Schema,
  requiredFields: Seq[String])
    extends Schema {
  override val isPrimitive: Boolean = schema.isPrimitive
  override def mandatory: Boolean = requiredFields.contains(name)
  override def validate: Boolean = schema.validate
}
