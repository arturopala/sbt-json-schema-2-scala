package uk.gov.hmrc.jsonschema2scala.schema

import java.net.URI

import play.api.libs.json.{JsArray, JsObject, JsString, JsValue}
import uk.gov.hmrc.jsonschema2scala.NameUtils

import scala.util.Try

object SchemaReader {

  def read(name: String, json: JsObject, references: Map[String, SchemaSource]): Schema =
    read(URI.create(s"schema://$name"), name, json, Some(SourceMapReferenceResolver(references)))

  def read(uri: URI, name: String, json: JsObject, references: Map[String, SchemaSource]): Schema =
    read(uri, name, json, Some(SourceMapReferenceResolver(references)))

  def read(uri: URI, name: String, json: JsObject, externalResolver: Option[SchemaReferenceResolver] = None): Schema =
    readSchema(
      name = name,
      currentPath = SchemaReferenceResolver.rootPath(uri),
      json = json,
      requiredFields = Seq(""),
      currentReferenceResolver = CachingReferenceResolver(uri, json, externalResolver)
    )

  def readMany(sources: Seq[SchemaSource]): Seq[Schema] = {
    val references: Map[String, SchemaSource] = sources
      .map(schema => (schema.uri.toString, schema))
      .toMap
    sources.map(source => read(source.uri, source.name, source.json, Some(SourceMapReferenceResolver(references))))
  }

  def readSchema(
    name: String,
    currentPath: List[String],
    json: JsObject,
    description: Option[String] = None,
    requiredFields: Seq[String],
    currentReferenceResolver: SchemaReferenceResolver): Schema = {

    val isMandatory = requiredFields.contains(name)

    val id: Option[URI] = readId(json)

    val (referenceResolver, path) =
      id.map(uri =>
          (CachingReferenceResolver(uri, json, Some(currentReferenceResolver)), SchemaReferenceResolver.rootPath(uri)))
        .getOrElse((currentReferenceResolver, currentPath))

    val desc: Option[String] = description.orElse((json \ "description").asOpt[String])

    val definitions: Seq[Schema] = readDefinitions("definitions", name, path, json, description, referenceResolver) ++
      readDefinitions("$defs", name, path, json, description, referenceResolver)

    val common = SchemaCommon(definitions)

    /*
     * 6.1. Validation Keywords for Any Instance Type
     * 6.1.1. type
     * The value of this keyword MUST be either a string or an array. If it is an array, elements of the array MUST be strings and MUST be unique.
     *
     * String values MUST be one of the six primitive types ("null", "boolean", "object", "array", "number", or "string"),
     * or "integer" which matches any number with a zero fractional part.
     *
     * An instance validates if and only if the instance is in any of the sets listed for this keyword.
     */
    (json \ "type").asOpt[JsValue] match {
      case Some(JsString(valueType)) =>
        valueType match {
          case "object" =>
            readObjectSchema(name, path, common, json, desc, isMandatory, referenceResolver)
          case "string"  => readStringSchema(name, path, common, json, desc, isMandatory)
          case "number"  => readNumberSchema(name, path, common, json, desc, isMandatory)
          case "integer" => readIntegerSchema(name, path, common, json, desc, isMandatory)
          case "boolean" => BooleanSchema(name, path, common, desc, mandatory = true)
          case "array" =>
            readArraySchema(name, path, common, json, desc, isMandatory, referenceResolver)
          case "null" =>
            readObjectSchema(name, path, common, json, desc, isMandatory, referenceResolver)
          case other =>
            throw new IllegalStateException(
              s"Invalid type name, expected one of [null, boolean, object, array, number, integer, string], but got $other")
        }
      case Some(JsArray(valueTypes)) =>
        val mandatory = requiredFields.contains(name)
        val variants: Seq[Schema] = valueTypes.distinct.map {
          case JsString(valueType) =>
            valueType match {
              case "object" =>
                ObjectSchema(name, path, common, description, mandatory, Seq.empty, Seq.empty)
              case "string" =>
                StringSchema(name, path, common, description, mandatory)
              case "number"  => NumberSchema(name, path, common, description, mandatory)
              case "integer" => IntegerSchema(name, path, common, description, mandatory)
              case "boolean" => BooleanSchema(name, path, common, desc, mandatory = true)
              case "array"   => throw new IllegalStateException(s"Unspecified 'array' type not supported yet!")
              case "null"    => NullSchema(name, path, common, desc)
              case other =>
                throw new IllegalStateException(
                  s"Invalid type name, expected one of [null, boolean, object, array, number, integer, string], but got $other")
            }
          case other =>
            throw new IllegalStateException(s"Invalid type definition, expected an array of strings, but got $other")

        }
        if (variants.isEmpty) throw new IllegalStateException(s"")
        else if (variants.size == 1) variants.head
        else OneOfSchema(name, path, common, description, mandatory, variants, Seq.empty)

      case Some(other) =>
        throw new IllegalStateException(
          s"Invalid type definition, expected a string or an array of strings but got $other")

      case None =>
        (json \ "$ref").asOpt[String] match {
          case Some(reference) =>
            val path2 = referenceResolver.uriToPath(reference)
            referenceResolver
              .lookup(reference, readSchema(_, path2, _, description = desc, requiredFields, referenceResolver)) match {
              case Some(referencedSchema) =>
                if (referenceResolver.isInternal(referencedSchema.uri)) {
                  if (referencedSchema.isPrimitive)
                    SchemaUtils.copy(referencedSchema, name, "$ref" :: path, description)
                  else
                    InternalSchemaReference(
                      name = name,
                      path = "$ref" :: path,
                      common = common,
                      description = description,
                      reference = reference,
                      schema = referencedSchema,
                      required = requiredFields)
                } else
                  ExternalSchemaReference(
                    name = name,
                    path = "$ref" :: path,
                    common = common,
                    description = description,
                    reference = reference,
                    schema = referencedSchema,
                    required = requiredFields)

              case None =>
                throw new IllegalStateException(s"Invalid schema reference $reference")
            }
          case None =>
            (json \ "oneOf").asOpt[JsArray] match {
              case Some(array) =>
                readOneOfSchema(
                  name,
                  "oneOf" :: path,
                  common,
                  array,
                  description,
                  isMandatory,
                  requiredFields,
                  Seq.empty,
                  referenceResolver)
              case None =>
                (json \ "const").asOpt[JsValue] match {
                  case Some(const) =>
                    const match {
                      case JsString(value) =>
                        StringSchema(
                          name = name,
                          path = "const" :: path,
                          common = common,
                          description = description,
                          mandatory = requiredFields.contains(name),
                          enum = Some(Seq(value)))
                      case other =>
                        throw new IllegalStateException(
                          s"Unsupported const definition, expected string value, but got $other.")
                    }
                  case None =>
                    (json \ "properties").asOpt[JsValue] match {
                      case Some(_) =>
                        // fallback to assume it is object schema if properties present
                        readObjectSchema(name, path, common, json, desc, isMandatory, referenceResolver)
                      case None =>
                        // fallback to assume it is object schema with indirect properties
                        readObjectSchema(
                          name,
                          path,
                          common,
                          JsObject(Seq("properties" -> json)),
                          desc,
                          isMandatory,
                          referenceResolver)
                    }
                }
            }
        }
    }
  }

  def readId(json: JsObject): Option[URI] =
    (json \ "$id")
      .asOpt[String]
      .flatMap(s => Try(URI.create(s)).toOption)
      .map(_.normalize())

  def readStringSchema(
    name: String,
    path: List[String],
    common: SchemaCommon,
    json: JsObject,
    description: Option[String] = None,
    isMandatory: Boolean): StringSchema = {

    val pattern = (json \ "pattern").asOpt[String]
    val enum = (json \ "enum").asOpt[Seq[String]]
    val minLength = (json \ "minLength").asOpt[Int]
    val maxLength = (json \ "maxLength").asOpt[Int]
    val isUniqueKey = (json \ "x_uniqueKey").asOpt[Boolean].getOrElse(false)
    val isKey = (json \ "x_key").asOpt[Boolean].getOrElse(false)
    val customGenerator = (json \ "x_gen").asOpt[String]

    StringSchema(
      name = name,
      path = path,
      common = common,
      description = description,
      mandatory = isMandatory,
      pattern = pattern,
      enum = enum,
      minLength = minLength,
      maxLength = maxLength,
      isUniqueKey = isUniqueKey,
      isKey = isKey,
      customGenerator = customGenerator
    )
  }

  def readNumberSchema(
    name: String,
    path: List[String],
    common: SchemaCommon,
    json: JsObject,
    description: Option[String] = None,
    isMandatory: Boolean): NumberSchema = {

    val minimum = (json \ "minimum").asOpt[BigDecimal]
    val maximum = (json \ "maximum").asOpt[BigDecimal]
    val multipleOf = (json \ "multipleOf").asOpt[BigDecimal]
    val customGenerator = (json \ "x_gen").asOpt[String]

    NumberSchema(
      name = name,
      path = path,
      common = common,
      description = description,
      mandatory = isMandatory,
      customGenerator = customGenerator,
      minimum = minimum,
      maximum = maximum,
      multipleOf = multipleOf
    )
  }

  def readIntegerSchema(
    name: String,
    path: List[String],
    common: SchemaCommon,
    json: JsObject,
    description: Option[String] = None,
    isMandatory: Boolean): IntegerSchema = {

    val minimum = (json \ "minimum").asOpt[Int]
    val maximum = (json \ "maximum").asOpt[Int]
    val multipleOf = (json \ "multipleOf").asOpt[Int]
    val customGenerator = (json \ "x_gen").asOpt[String]

    IntegerSchema(
      name = name,
      path = path,
      common = common,
      description = description,
      mandatory = isMandatory,
      customGenerator = customGenerator,
      minimum = minimum,
      maximum = maximum,
      multipleOf = multipleOf
    )
  }

  def readObjectSchema(
    name: String,
    path: List[String],
    common: SchemaCommon,
    json: JsObject,
    description: Option[String] = None,
    isMandatory: Boolean,
    referenceResolver: SchemaReferenceResolver): Schema = {

    val (required, alternatives) = readRequiredProperty(json)

    val propertiesOpt: Option[Seq[Schema]] = (json \ "properties")
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
                    currentPath = name :: "properties" :: path,
                    json = fieldProperty,
                    requiredFields = required,
                    currentReferenceResolver = referenceResolver)
                case other =>
                  throw new IllegalArgumentException(s"Invalid object schema, expected ${path.reverse
                    .mkString("/")}/$name to be an object, but got ${other.getClass.getSimpleName}")
              }
            }))

    val patternPropertiesOpt: Option[Seq[Schema]] = (json \ "patternProperties")
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
                      currentPath = pattern :: "patternProperties" :: path,
                      json = fieldProperty,
                      requiredFields = required,
                      currentReferenceResolver = referenceResolver)
                  case other =>
                    throw new IllegalArgumentException(s"Invalid object schema, expected ${path.reverse
                      .mkString("/")}/$pattern to be an object, but got ${other.getClass.getSimpleName}")
                }
              }
          })

    val additionalPropertiesOpt: Option[Seq[Schema]] = (json \ "additionalProperties")
      .asOpt[JsValue] match {
      case Some(property: JsObject) =>
        val definition = readSchema(
          name = name,
          currentPath = "additionalProperties" :: path,
          json = property,
          requiredFields = required,
          currentReferenceResolver = referenceResolver)
        definition match {
          case o: ObjectSchema => Some(o.properties)
          case m: MapSchema    => Some(m.patternProperties)
          case other           => Some(Seq(other))
        }

      case _ => None
    }

    (propertiesOpt, patternPropertiesOpt, additionalPropertiesOpt) match {
      case (Some(properties), _, _) =>
        ObjectSchema(
          name = name,
          path = path,
          common = common,
          description = description,
          mandatory = isMandatory,
          properties = properties ++ additionalPropertiesOpt.getOrElse(Seq.empty),
          required = required,
          alternatives = alternatives,
          patternProperties = patternPropertiesOpt
        )

      case (None, _, Some(additionalProperties)) =>
        ObjectSchema(
          name = name,
          path = path,
          common = common,
          description = description,
          mandatory = isMandatory,
          properties = additionalProperties,
          required = required,
          alternatives = alternatives,
          patternProperties = patternPropertiesOpt
        )

      case (None, Some(patternProperties), None) =>
        MapSchema(
          name = name,
          path = path,
          common = common,
          description = description,
          mandatory = isMandatory,
          patternProperties = patternProperties,
          requiredFields = required,
          alternatives = alternatives
        )

      case (None, None, _) =>
        (json \ "oneOf").asOpt[JsArray] match {
          case Some(array) =>
            readOneOfSchema(
              name,
              "oneOf" :: path,
              common,
              array,
              description,
              isMandatory,
              required,
              alternatives,
              referenceResolver)
          case None =>
            ObjectSchema(
              name = name,
              path = path,
              common = common,
              description = description,
              mandatory = isMandatory,
              properties = additionalPropertiesOpt.getOrElse(Seq.empty),
              required = required,
              alternatives = alternatives,
              patternProperties = None
            )
        }
    }
  }

  def readArraySchema(
    name: String,
    path: List[String],
    common: SchemaCommon,
    json: JsObject,
    description: Option[String] = None,
    isMandatory: Boolean,
    referenceResolver: SchemaReferenceResolver): ArraySchema = {

    val items = (json \ "items").as[JsObject]
    val minItems = (json \ "minItems").asOpt[Int]
    val maxItems = (json \ "maxItems").asOpt[Int]

    val itemDefinition =
      readSchema(
        NameUtils.singular(name),
        "items" :: path,
        items,
        requiredFields = Seq(name),
        currentReferenceResolver = referenceResolver)

    ArraySchema(
      name = name,
      path = path,
      common = common,
      description = description,
      mandatory = isMandatory,
      item = itemDefinition,
      minItems = minItems,
      maxItems = maxItems)
  }

  def readOneOfSchema(
    name: String,
    path: List[String],
    common: SchemaCommon,
    array: JsArray,
    description: Option[String] = None,
    isMandatory: Boolean,
    required: Seq[String],
    alternatives: Seq[Set[String]],
    referenceResolver: SchemaReferenceResolver): OneOfSchema = {
    val props = array.value.zipWithIndex.map {
      case (jsObject: JsObject, i) =>
        readSchema(
          name,
          i.toString :: path,
          jsObject,
          requiredFields = required,
          currentReferenceResolver = referenceResolver)
      case (other, i) =>
        throw new IllegalArgumentException(
          s"Invalid oneOf schema, expected ${path.reverse.mkString("/")}[$i] to be an object, but got ${other.getClass.getSimpleName}")
    }
    OneOfSchema(name, path, common, description = description, isMandatory, variants = props, alternatives)
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

  def readRequiredProperty(json: JsObject): (Seq[String], Seq[Set[String]]) =
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
