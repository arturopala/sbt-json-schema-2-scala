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

import java.net.URI

import play.api.libs.json.{JsArray, JsObject, JsString, JsValue}

import scala.util.Try

object SchemaReader {

  def read(name: String, json: JsObject): Schema = {
    val uri = attemptReadId(json).getOrElse(URI.create(s"schema://$name"))
    read(uri, name, json, None)
  }

  def read(name: String, json: JsObject, references: Map[String, SchemaSource]): Schema = {
    val uri = attemptReadId(json).getOrElse(URI.create(s"schema://$name"))
    read(uri, name, json, Some(SourceMapReferenceResolver(references)))
  }

  def read(uri: URI, name: String, json: JsObject, references: Map[String, SchemaSource]): Schema =
    read(uri, name, json, Some(SourceMapReferenceResolver(references)))

  def readMany(sources: Seq[SchemaSource]): Seq[Schema] = {
    val references: Map[String, SchemaSource] = sources
      .map(schema => (schema.uri.toString, schema))
      .toMap
    sources.map(source => read(source.uri, source.name, source.json, Some(SourceMapReferenceResolver(references))))
  }

  def read(uri: URI, name: String, json: JsObject, externalResolver: Option[SchemaReferenceResolver] = None): Schema = {
    val resolver = CachingReferenceResolver(uri, json, externalResolver)
    val path = SchemaReferenceResolver.rootPath(uri)
    resolver.lookup(uri.toString, (_, j) => readSchema(name, path, j, None, Seq(""), resolver)) match {
      case None         => throw new IllegalStateException(s"Unexpected error, schema lookup failed for $uri")
      case Some(schema) => schema
    }
  }

  case class Parameters(
    name: String,
    path: List[String],
    common: SchemaCommon,
    json: JsObject,
    description: Option[String],
    isMandatory: Boolean,
    requiredFields: Seq[String],
    referenceResolver: SchemaReferenceResolver)

  def readSchema(
    name: String,
    currentPath: List[String],
    json: JsObject,
    externalDescription: Option[String] = None,
    requiredFields: Seq[String],
    currentReferenceResolver: SchemaReferenceResolver): Schema = {

    val isMandatory = requiredFields.contains(name)

    val id: Option[URI] = attemptReadId(json)

    val (referenceResolver, path) =
      decideResolverAndPath(currentPath, json, currentReferenceResolver, id)

    val description: Option[String] = externalDescription.orElse(attemptReadDescription(json))

    val definitions: Seq[Schema] = readDefinitions("definitions", name, path, json, description, referenceResolver) ++
      readDefinitions("$defs", name, path, json, description, referenceResolver)

    val common = SchemaCommon(definitions)

    val p = Parameters(name, path, common, json, description, isMandatory, requiredFields, referenceResolver)

    attemptReadByType(p)
      .orElse { attemptReadReference(p) }
      .orElse { attemptReadOneOf(p) }
      .orElse { attemptReadObjectIfProperties(p) }
      .orElse { attemptReadConstant(p) }
      .orElse { attemptReadEnum(p) }
      .getOrElse {
        // fallback to read object schema with implicit properties
        readObjectSchema(p.copy(json = JsObject(Seq("properties" -> json))))
      }
  }

  def attemptReadId(json: JsObject): Option[URI] =
    (json \ "$id")
      .asOpt[String]
      .flatMap(s => Try(URI.create(s)).toOption)
      .map(_.normalize())

  def decideResolverAndPath(
    path: List[String],
    json: JsObject,
    referenceResolver: SchemaReferenceResolver,
    id: Option[URI]): (SchemaReferenceResolver, List[String]) =
    id.map { uri =>
        val uri2 = if (uri.isAbsolute) uri else referenceResolver.resolveUri(uri)
        (CachingReferenceResolver(uri2, json, Some(referenceResolver)), SchemaReferenceResolver.rootPath(uri))
      }
      .getOrElse((referenceResolver, path))

  def attemptReadDescription(json: JsObject): Option[String] =
    (json \ "description").asOpt[String]

  /*
    6.1.1. type
    The value of this keyword MUST be either a string or an array.
    If it is an array, elements of the array MUST be strings and MUST be unique.
    String values MUST be one of the six primitive types
    ("null", "boolean", "object", "array", "number", or "string"),
    or "integer" which matches any number with a zero fractional part.
   */
  def attemptReadByType(p: Parameters): Option[Schema] =
    (p.json \ "type")
      .asOpt[JsValue]
      .map {
        case JsString(schemaType)     => readSchemaWithType(p, schemaType)
        case JsArray(schemaTypeArray) => readSchemaWithTypeArray(p, schemaTypeArray)
        case other =>
          throw new IllegalStateException(
            s"Invalid type definition, expected a string or an array of strings but got $other")
      }

  def readSchemaWithType(p: Parameters, schemaType: String): Schema =
    schemaType match {
      case "string"  => readStringSchema(p)
      case "number"  => readNumberSchema(p)
      case "integer" => readIntegerSchema(p)
      case "boolean" => BooleanSchema(p.name, p.path, p.common, p.description, mandatory = true)
      case "object"  => readObjectSchema(p)
      case "array"   => readArraySchema(p)
      case "null"    => readObjectSchema(p)
      case other =>
        throw new IllegalStateException(
          s"Invalid type name, expected one of [null, boolean, object, array, number, integer, string], but got $other")
    }

  def readSchemaWithTypeArray(p: Parameters, schemaTypeArray: Seq[JsValue]): Schema = {

    val variants: Seq[Schema] = schemaTypeArray.distinct.map {
      case JsString(valueType) =>
        valueType match {
          case "string" =>
            StringSchema(p.name, p.path, p.common, p.description, p.isMandatory)
          case "number"  => NumberSchema(p.name, p.path, p.common, p.description, p.isMandatory)
          case "integer" => IntegerSchema(p.name, p.path, p.common, p.description, p.isMandatory)
          case "boolean" => BooleanSchema(p.name, p.path, p.common, p.description, mandatory = true)
          case "object" =>
            ObjectSchema(p.name, p.path, p.common, p.description, p.isMandatory, Seq.empty, Seq.empty)
          case "null" => NullSchema(p.name, p.path, p.common, p.description)
          case "array" =>
            ArraySchema(p.name, p.path, p.common, p.description, p.isMandatory)
          case other =>
            throw new IllegalStateException(
              s"Invalid type name, expected one of [null, boolean, object, array, number, integer, string], but got $other")
        }
      case other =>
        throw new IllegalStateException(s"Invalid type definition, expected an array of strings, but got $other")

    }
    if (variants.isEmpty) throw new IllegalStateException(s"")
    else if (variants.size == 1) variants.head
    else OneOfSchema(p.name, p.path, p.common, p.description, p.isMandatory, variants, Seq.empty)
  }

  def attemptReadReference(p: Parameters): Option[Schema] =
    (p.json \ "$ref")
      .asOpt[String]
      .map { reference =>
        resolveReference(p, reference)
      }

  def resolveReference(p: Parameters, reference: String): Schema = {

    val path2 = p.referenceResolver.uriToPath(reference)

    p.referenceResolver
      .lookup(
        reference,
        readSchema(_, path2, _, externalDescription = p.description, p.requiredFields, p.referenceResolver)) match {

      case Some(referencedSchema) =>
        if (p.referenceResolver.isInternal(referencedSchema.uri)) {
          if (referencedSchema.isPrimitive)
            SchemaUtils.copy(referencedSchema, p.name, "$ref" :: p.path, p.description)
          else
            InternalSchemaReference(
              name = p.name,
              path = "$ref" :: p.path,
              common = p.common,
              description = p.description,
              reference = reference,
              schema = referencedSchema,
              requiredFields = p.requiredFields)
        } else
          ExternalSchemaReference(
            name = p.name,
            path = "$ref" :: p.path,
            common = p.common,
            description = p.description,
            reference = reference,
            schema = referencedSchema,
            requiredFields = p.requiredFields)

      case None =>
        throw new IllegalStateException(s"Invalid schema reference $reference")
    }
  }

  def attemptReadOneOf(p: Parameters, alternativeRequiredFields: Seq[Set[String]] = Seq.empty): Option[OneOfSchema] =
    (p.json \ "oneOf")
      .asOpt[JsArray]
      .map { array =>
        readOneOfSchema(p, array, alternativeRequiredFields)
      }

  def readOneOfSchema(p: Parameters, array: JsArray, alternativeRequiredFields: Seq[Set[String]]): OneOfSchema = {
    val props = array.value.zipWithIndex.map {
      case (jsObject: JsObject, i) =>
        readSchema(
          p.name,
          i.toString :: p.path,
          jsObject,
          requiredFields = p.requiredFields,
          currentReferenceResolver = p.referenceResolver)
      case (other, i) =>
        throw new IllegalArgumentException(
          s"Invalid oneOf schema, expected ${p.path.reverse.mkString("/")}[$i] to be an object, but got ${other.getClass.getSimpleName}")
    }
    OneOfSchema(
      name = p.name,
      path = p.path,
      common = p.common,
      description = p.description,
      mandatory = p.isMandatory,
      variants = props,
      alternativeRequiredFields = alternativeRequiredFields
    )
  }

  def attemptReadObjectIfProperties(p: Parameters): Option[Schema] =
    (p.json \ "properties")
      .asOpt[JsValue]
      .map { _ =>
        readObjectSchema(p)
      }

  /*
    6.1.3. const
    The value of this keyword MAY be of any type, including null.
    Use of this keyword is functionally equivalent to an "enum" with a single value.
   */
  def attemptReadConstant(p: Parameters): Option[Schema] =
    (p.json \ "const")
      .asOpt[JsValue]
      .map {
        case JsString(value) =>
          StringSchema(
            name = p.name,
            path = p.path,
            common = p.common,
            description = p.description,
            mandatory = p.isMandatory,
            enum = Some(Seq(value)))

        case json: JsObject =>
          readObjectSchema(p.copy(json = json))

        case other =>
          throw new IllegalStateException(
            s"Unsupported const definition, expected a string or an object, but got $other.")
      }

  /*
    6.1.2. enum
    The value of this keyword MUST be an array.
    This array SHOULD have at least one element.
    Elements in the array SHOULD be unique.
    Elements in the array might be of any type, including null.
   */
  def attemptReadEnum(p: Parameters): Option[Schema] =
    (p.json \ "enum")
      .asOpt[JsArray]
      .map {
        case jsArray @ JsArray(array) if array.nonEmpty =>
          if (array.forall(_.isInstanceOf[JsString])) {
            StringSchema(
              name = p.name,
              path = p.path,
              common = p.common,
              description = p.description,
              mandatory = p.isMandatory,
              enum = Some(array.map(_.as[String])))
          } else if (array.forall(_.isInstanceOf[JsObject])) {
            readOneOfSchema(p, jsArray, Seq.empty)
          } else {
            throw new IllegalStateException(
              s"Invalid enum definition, expected an array of strings or objects, but got $jsArray.")
          }

        case other =>
          throw new IllegalStateException(s"Invalid enum definition, expected a non-empty array, but got $other.")
      }

  def readStringSchema(p: Parameters): StringSchema = {

    val pattern = (p.json \ "pattern").asOpt[String]
    val enum = (p.json \ "enum").asOpt[Seq[String]]
    val minLength = (p.json \ "minLength").asOpt[Int]
    val maxLength = (p.json \ "maxLength").asOpt[Int]
    val isUniqueKey = (p.json \ "x_uniqueKey").asOpt[Boolean].getOrElse(false)
    val isKey = (p.json \ "x_key").asOpt[Boolean].getOrElse(false)
    val customGenerator = (p.json \ "x_gen").asOpt[String]

    StringSchema(
      name = p.name,
      path = p.path,
      common = p.common,
      description = p.description,
      mandatory = p.isMandatory,
      pattern = pattern,
      enum = enum,
      minLength = minLength,
      maxLength = maxLength,
      isUniqueKey = isUniqueKey,
      isKey = isKey,
      customGenerator = customGenerator
    )
  }

  def readNumberSchema(p: Parameters): NumberSchema = {

    val minimum = (p.json \ "minimum").asOpt[BigDecimal]
    val maximum = (p.json \ "maximum").asOpt[BigDecimal]
    val multipleOf = (p.json \ "multipleOf").asOpt[BigDecimal]
    val customGenerator = (p.json \ "x_gen").asOpt[String]

    NumberSchema(
      name = p.name,
      path = p.path,
      common = p.common,
      description = p.description,
      mandatory = p.isMandatory,
      customGenerator = customGenerator,
      minimum = minimum,
      maximum = maximum,
      multipleOf = multipleOf
    )
  }

  def readIntegerSchema(p: Parameters): IntegerSchema = {

    val minimum = (p.json \ "minimum").asOpt[Int]
    val maximum = (p.json \ "maximum").asOpt[Int]
    val multipleOf = (p.json \ "multipleOf").asOpt[Int]
    val customGenerator = (p.json \ "x_gen").asOpt[String]

    IntegerSchema(
      name = p.name,
      path = p.path,
      common = p.common,
      description = p.description,
      mandatory = p.isMandatory,
      customGenerator = customGenerator,
      minimum = minimum,
      maximum = maximum,
      multipleOf = multipleOf
    )
  }

  def readObjectSchema(p: Parameters): Schema = {

    val (required, alternatives) = readRequired(p.json)
    val p2 = p.copy(requiredFields = required)

    val propertiesOpt: Option[Seq[Schema]] =
      attemptReadProperties(p2)

    val patternPropertiesOpt: Option[Seq[Schema]] =
      attemptReadPatternProperties(p2)

    val additionalPropertiesOpt: Option[Seq[Schema]] =
      attemptReadAdditionalProperties(p2)

    (propertiesOpt, patternPropertiesOpt, additionalPropertiesOpt) match {
      case (Some(properties), _, _) =>
        ObjectSchema(
          name = p2.name,
          path = p2.path,
          common = p2.common,
          description = p2.description,
          mandatory = p2.isMandatory,
          properties = properties ++ additionalPropertiesOpt.getOrElse(Seq.empty),
          requiredFields = required,
          alternativeRequiredFields = alternatives,
          patternProperties = patternPropertiesOpt
        )

      case (None, _, Some(additionalProperties)) =>
        ObjectSchema(
          name = p2.name,
          path = p2.path,
          common = p2.common,
          description = p2.description,
          mandatory = p2.isMandatory,
          properties = additionalProperties,
          requiredFields = required,
          alternativeRequiredFields = alternatives,
          patternProperties = patternPropertiesOpt
        )

      case (None, Some(patternProperties), None) =>
        MapSchema(
          name = p2.name,
          path = p2.path,
          common = p2.common,
          description = p2.description,
          mandatory = p2.isMandatory,
          patternProperties = patternProperties,
          requiredFields = required,
          alternativeRequiredFields = alternatives
        )

      case (None, None, _) =>
        attemptReadOneOf(p2, alternatives)
          .getOrElse(
            ObjectSchema(
              name = p2.name,
              path = p2.path,
              common = p2.common,
              description = p2.description,
              mandatory = p2.isMandatory,
              properties = additionalPropertiesOpt.getOrElse(Seq.empty),
              requiredFields = required,
              alternativeRequiredFields = alternatives,
              patternProperties = None
            ))
    }
  }

  def attemptReadProperties(p: Parameters): Option[Seq[Schema]] =
    (p.json \ "properties")
      .asOpt[JsObject]
      .map(
        properties =>
          properties.fields
            .map(_._1)
            .distinct
            .map(name => {
              (properties \ name).as[JsValue] match {
                case fieldProperty: JsObject =>
                  readSchema(
                    name = name,
                    currentPath = name :: "properties" :: p.path,
                    json = fieldProperty,
                    requiredFields = p.requiredFields,
                    currentReferenceResolver = p.referenceResolver)
                case other =>
                  throw new IllegalArgumentException(s"Invalid object schema, expected ${p.path.reverse
                    .mkString("/")}/$name to be an object, but got ${other.getClass.getSimpleName}")
              }
            }))

  def attemptReadPatternProperties(p: Parameters): Option[Seq[Schema]] =
    (p.json \ "patternProperties")
      .asOpt[JsObject]
      .map(
        patternProperties =>
          patternProperties.fields
            .map(_._1)
            .distinct
            .map { pattern =>
              {
                (patternProperties \ pattern).as[JsValue] match {
                  case fieldProperty: JsObject =>
                    readSchema(
                      name = pattern,
                      currentPath = pattern :: "patternProperties" :: p.path,
                      json = fieldProperty,
                      requiredFields = p.requiredFields,
                      currentReferenceResolver = p.referenceResolver
                    )
                  case other =>
                    throw new IllegalArgumentException(s"Invalid object schema, expected ${p.path.reverse
                      .mkString("/")}/$pattern to be an object, but got ${other.getClass.getSimpleName}")
                }
              }
          })

  def attemptReadAdditionalProperties(p: Parameters): Option[Seq[Schema]] =
    (p.json \ "additionalProperties")
      .asOpt[JsValue] match {
      case Some(property: JsObject) =>
        val definition = readSchema(
          name = p.name,
          currentPath = "additionalProperties" :: p.path,
          json = property,
          requiredFields = p.requiredFields,
          currentReferenceResolver = p.referenceResolver)
        definition match {
          case o: ObjectSchema => Some(o.properties)
          case m: MapSchema    => Some(m.patternProperties)
          case other           => Some(Seq(other))
        }

      case _ => None
    }

  def readArraySchema(p: Parameters): ArraySchema = {

    val minItems = (p.json \ "minItems").asOpt[Int]
    val maxItems = (p.json \ "maxItems").asOpt[Int]

    /*
      9.3.1.1. items
      The value of "items" MUST be either a valid JSON Schema or an array of valid JSON Schemas.
     */
    val itemDefinition = (p.json \ "items")
      .asOpt[JsValue]
      .map({
        case itemSchema: JsObject =>
          readSchema(
            NameUtils.singular(p.name),
            "items" :: p.path,
            itemSchema,
            requiredFields = Seq(p.name),
            currentReferenceResolver = p.referenceResolver)

        case _: JsArray =>
          throw new IllegalStateException("Unsupported feature, array with items of an array shape")

        case other =>
          throw new IllegalStateException(s"Invalid schema, expected object or an array, but got $other")
      })

    ArraySchema(
      name = p.name,
      path = p.path,
      common = p.common,
      description = p.description,
      mandatory = p.isMandatory,
      item = itemDefinition,
      minItems = minItems,
      maxItems = maxItems)
  }

  def readDefinitions(
    propertyName: String,
    name: String,
    path: List[String],
    json: JsObject,
    description: Option[String] = None,
    referenceResolver: SchemaReferenceResolver): Seq[Schema] =
    (json \ propertyName)
      .asOpt[JsObject]
      .map(
        properties =>
          properties.fields
            .map(_._1)
            .distinct
            .map(name => {
              val uri = SchemaReferenceResolver.pathToUri(name :: propertyName :: path)
              val path2 = referenceResolver.uriToPath(uri)
              referenceResolver
                .lookup(uri, readSchema(_, path2, _, description, Seq.empty, referenceResolver))
            })
            .collect { case Some(x) => x }
      )
      .getOrElse(Seq.empty)

  def readRequired(json: JsObject): (Seq[String], Seq[Set[String]]) =
    (json \ "required")
      .asOpt[Seq[String]]
      .map(s => (s, Seq.empty))
      .orElse(
        (json \ "oneOf")
          .asOpt[Seq[JsObject]]
          .map(s => {
            val names = s.map(o => (o \ "required").asOpt[Set[String]].getOrElse(Set.empty))
            val required = names.reduce(_ intersect _)
            val variants = names.map(_ -- required)
            (required.toSeq, variants)
          })
      )
      .getOrElse((Seq.empty, Seq.empty))
}
