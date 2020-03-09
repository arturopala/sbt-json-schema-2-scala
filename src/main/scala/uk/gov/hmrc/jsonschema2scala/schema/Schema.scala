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

package uk.gov.hmrc.jsonschema2scala.schema

import java.net.{URI, URLDecoder}

import play.api.libs.json.{JsValue, Reads}

sealed trait Schema {

  final def name: String = attributes.name
  final def path: List[String] = attributes.path
  final def definitions: Seq[Schema] = attributes.definitions
  final def description: Option[String] =
    attributes.description
      .orElse(resolved.attributes.description)

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

  def uriDecoded: String = URLDecoder.decode(uri, "utf-8")

  lazy val resolved: Schema = this

  def addDefinitions(definitions: Seq[Schema]): Schema =
    SchemaUtils.copyAttributes(this, attributes.copy(definitions = (attributes.definitions ++ definitions).distinct))

  def withName(name: String): Schema =
    SchemaUtils.copyAttributes(this, attributes.copy(name = name))

  override def toString: String =
    s"${this.getClass.getSimpleName} name:$name${if (definitions.nonEmpty)
      s" defs:[${definitions.map(s => s"${s.name}:${s.getClass.getSimpleName}@${s.uri}").mkString(", ")}]"
    else ""}"
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

  override def toString: String =
    super.toString + items
      .map(_.map(i => s"${i.getClass.getSimpleName}@${i.uri}").mkString(" items:[", ", ", "]"))
      .getOrElse("")
}

case class ObjectSchema(
  attributes: SchemaAttributes,
  properties: Seq[Schema] = Seq.empty,
  requiredFields: Seq[String] = Seq.empty,
  alternativeRequiredFields: Seq[Set[String]] = Seq.empty,
  patternProperties: Option[Seq[Schema]] = None,
  additionalProperties: Option[Schema] = None,
  unevaluatedProperties: Option[Schema] = None,
  minProperties: Option[Int] = None,
  maxProperties: Option[Int] = None)
    extends Schema {

  final val primitive: Boolean = false
  final val validated: Boolean = true

  def isEmpty: Boolean = !hasExplicitProperties && !hasRequiredFields && !hasPropertiesNumberLimit

  def hasExplicitProperties: Boolean =
    properties.nonEmpty || patternProperties.nonEmpty ||
      additionalProperties.nonEmpty || unevaluatedProperties.nonEmpty

  def hasRequiredFields: Boolean = requiredFields.nonEmpty || alternativeRequiredFields.exists(_.nonEmpty)

  def hasPropertiesNumberLimit: Boolean = minProperties.isDefined || (maxProperties.isDefined && maxProperties.get > 0)

  def hasImplicitPropertiesOnly: Boolean =
    hasRequiredFields && !hasExplicitProperties && !hasPropertiesNumberLimit

  def hasSingleCollectiveFieldOnly: Boolean =
    properties.isEmpty && implicitProperties.isEmpty && collectiveFields.size == 1

  lazy val collectiveFields: Seq[(String, Schema)] =
    if (hasPropertiesNumberLimit && patternProperties.isEmpty && additionalProperties.isEmpty && unevaluatedProperties.isEmpty) {
      Seq(
        (
          "minMaxProperties",
          NullSchema(
            SchemaAttributes(
              name = "minMaxProperties",
              path = "minMaxProperties" :: attributes.path,
              description = None,
              required = true,
              custom = None))))
    } else {
      Seq(
        ("additionalProperties", additionalProperties),
        ("unevaluatedProperties", unevaluatedProperties),
      ).collect { case (id, Some(schema)) => (id, schema) } ++ patternProperties
        .map {
          _.sortBy(_.name)
            .map(schema => (s"patternProperties/${schema.name}", schema))
        }
        .getOrElse(Seq.empty)
    }

  val implicitProperties: Seq[Schema] = {

    def findImplicitProperties: Seq[(String, Int)] =
      if (hasImplicitPropertiesOnly) requiredFields.zipWithIndex
      else if (additionalProperties.isDefined || unevaluatedProperties.isDefined) Seq.empty
      else {
        requiredFields.zipWithIndex.filterNot {
          case (name, _) =>
            properties.exists(_.name == name) ||
              patternProperties.exists(_.exists(s => name.matches(s.name)))
        }
      }

    findImplicitProperties.map {
      case (name, index) =>
        NullSchema(
          SchemaAttributes(
            name = name,
            path = index.toString :: "required" :: attributes.path,
            description = None,
            required = true,
            custom = None))
    }
  }

  val namedProperties: Seq[Schema] = properties ++ implicitProperties

  override def toString: String =
    super.toString + (if (namedProperties.nonEmpty)
                        namedProperties
                          .map(s => s"${s.name}:${s.getClass.getSimpleName}")
                          .mkString(" props:[", ", ", "]")
                      else "")
}

case class OneOfAnyOfSchema(
  attributes: SchemaAttributes,
  variants: Seq[Schema] = Seq.empty,
  alternativeRequiredFields: Seq[Set[String]] = Seq.empty,
  isOneOf: Boolean)
    extends Schema {

  override val primitive: Boolean = variants.forall(_.primitive)
  override val validated: Boolean = variants.nonEmpty

  override def toString: String =
    super.toString +
      s"${variants.map(s => s"${s.getClass.getSimpleName}@${s.uri}").mkString(s" variants:(${variants.size})[", ", ", "]")}"
}

case class AllOfSchema(
  attributes: SchemaAttributes,
  partials: Seq[Schema] = Seq.empty,
  requiredFields: Seq[String] = Seq.empty,
  aggregatedSchema: Schema)
    extends Schema {

  override val primitive: Boolean = aggregatedSchema.primitive
  override val validated: Boolean = partials.nonEmpty

  override def toString: String =
    super.toString +
      s"${partials.map(s => s"${s.getClass.getSimpleName}@${s.uri}").mkString(s" parts:(${partials.size})[", ", ", "]")}"
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

  def isNonStandard: Boolean = !(reference.contains("$defs") || reference.contains("definitions"))

  override lazy val resolved: Schema = schema.resolved

  override def toString: String = super.toString + s" ref:$reference resolvedTo:{$schema}"
}

case class ExternalSchemaReference(
  attributes: SchemaAttributes,
  reference: String,
  schema: Schema,
  requiredFields: Seq[String],
  rootSchema: Option[Schema])
    extends Schema {

  override val primitive: Boolean = schema.primitive
  override val required: Boolean = requiredFields.contains(name)
  override val validated: Boolean = schema.validated

  override lazy val resolved: Schema = schema.resolved

  override def toString: String = super.toString + s" ref:$reference resolvedTo:{$schema}"
}

case class SchemaStub(attributes: SchemaAttributes, reference: String, resolver: SchemaReferenceResolver)
    extends Schema {

  override val required: Boolean = false
  override val validated: Boolean = false
  override val primitive: Boolean = false

  override lazy val resolved: Schema =
    resolver
      .lookupSchema(URI.create(reference), None)
      .flatMap {
        case (_: SchemaStub, _) => None
        case (schema, _)        => Some(schema.resolved)
      }
      .getOrElse(throw new IllegalStateException(
        s"Resolving type of the schema stub reference $reference, but the target schema not found in $resolver"))

  override def toString: String = super.toString + s" ref:$reference"

}

object SchemaStub {
  def apply(schema: Schema, reference: String, resolver: SchemaReferenceResolver): SchemaStub =
    new SchemaStub(schema.attributes, reference, resolver)
}
