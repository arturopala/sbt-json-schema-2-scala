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

import play.api.libs.json._

import scala.util.Try

object SchemaReader {

  def read(name: String, json: JsObject): Schema = {
    val uri = attemptReadId(json).getOrElse(URI.create(name))
    read(uri, name, json, None)
  }

  def read(name: String, json: JsObject, otherSchemas: Seq[SchemaSource]): Schema = {
    val uri = attemptReadId(json).getOrElse(URI.create(name))
    read(uri, name, json, Some(MultiSourceReferenceResolver(otherSchemas)))
  }

  def read(uri: URI, name: String, json: JsObject, otherSchemas: Seq[SchemaSource]): Schema =
    read(uri, name, json, Some(MultiSourceReferenceResolver(otherSchemas)))

  def readMany(sources: Seq[SchemaSource]): Seq[Schema] =
    sources.map(source =>
      source.json.fold(throw _, read(source.uri, source.name, _, Some(MultiSourceReferenceResolver(sources)))))

  def read(uri: URI, name: String, json: JsObject, externalResolver: Option[SchemaReferenceResolver] = None): Schema = {
    val resolver = CachingReferenceResolver(uri, name, json, externalResolver)
    val path = SchemaReferenceResolver.rootPath(uri)
    resolver.lookupSchema(uri, (_, j, r) => readSchema(name, path, j, None, Seq.empty, r)) match {
      case None =>
        throw new IllegalStateException(s"Unexpected error, schema lookup failed for $uri")

      case Some(schema) => schema
    }
  }

  case class Parameters(
    name: String,
    path: List[String],
    description: Option[String],
    definitions: Seq[Schema],
    required: Boolean,
    custom: Option[Map[String, JsValue]],
    json: JsObject,
    requiredFields: Seq[String],
    referenceResolver: SchemaReferenceResolver) {

    val a: SchemaAttributes =
      SchemaAttributes(name, path, description, definitions, required, custom)

    def currentUri: String = SchemaReferenceResolver.pathToReference(path)
  }

  val keywordsNotInVocabulary: Seq[(String, JsValue)] => Set[String] =
    Vocabulary.keywordsNotIn(Vocabulary.allKeywords)

  val keywordsInVocabularyNotMeta: Seq[(String, JsValue)] => Set[String] =
    Vocabulary.keywordsIn(Vocabulary.allKeywordsButMeta)

  def readSchema(
    name: String,
    currentPath: List[String],
    value: JsValue,
    externalDescription: Option[String] = None,
    requiredFields: Seq[String],
    currentReferenceResolver: SchemaReferenceResolver): Schema = value match {

    case boolean: JsBoolean =>
      boolean.value match {
        case true =>
          ObjectSchema(SchemaAttributes(name, currentPath, None, Seq.empty, false, None))

        case false =>
          val a = SchemaAttributes(name, currentPath, None, Seq.empty, false, None)
          NotSchema(a, ObjectSchema(a))
      }

    case json: JsObject =>
      val isMandatory = requiredFields.contains(name)

      val id: Option[URI] = attemptReadId(json)

      val (referenceResolver, path) =
        decideResolverAndPath(name, currentPath, json, currentReferenceResolver, id)

      val description: Option[String] = externalDescription.orElse(attemptReadDescription(json))

      val definitions: Seq[Schema] = readDefinitions("definitions", name, path, json, description, referenceResolver) ++
        readDefinitions("$defs", name, path, json, description, referenceResolver)

      val custom: Option[Map[String, JsValue]] = {
        val set = keywordsNotInVocabulary(json.fields)
        if (set.isEmpty) None else Some(set.map(k => (k, (json \ k).as[JsValue])).toMap)
      }

      val p =
        Parameters(name, path, description, definitions, isMandatory, custom, json, requiredFields, referenceResolver)

      attemptReadExplicitType(p)
        .orElse { attemptReadReference(p) }
        .orElse { attemptReadOneOf(p) }
        .orElse { attemptReadAnyOf(p) }
        .orElse { attemptReadAllOf(p) }
        .orElse { attemptReadNot(p) }
        .orElse { attemptReadIfThenElse(p) }
        .orElse { attemptReadImplicitType(p) }
        .getOrElse {
          val ks = keywordsInVocabularyNotMeta(json.fields)
          if (ks.nonEmpty) {
            throw new IllegalStateException(s"Unsupported schema feature(s): ${ks.mkString("|")}.")
          } else {
            // fallback to an empty object schema
            ObjectSchema(
              attributes = p.a,
              requiredFields = p.requiredFields
            )
          }
        }

    case other =>
      throw new IllegalStateException(s"Invalid schema, expected an object or boolean but got $other at $currentPath")
  }

  def attemptReadId(json: JsObject): Option[URI] =
    (json \ "$id")
      .asOpt[String]
      .orElse {
        (json \ "id")
          .asOpt[String]
      }
      .flatMap(s => {
        val s2 = if (s.endsWith("#")) s.dropRight(1) else s
        Try(URI.create(s2)).toOption
      })
      .map(_.normalize())

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
  def attemptReadExplicitType(p: Parameters): Option[Schema] =
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
      case "boolean" => readBooleanSchema(p)
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
          case "string"  => StringSchema(p.a)
          case "number"  => NumberSchema(p.a)
          case "integer" => IntegerSchema(p.a)
          case "boolean" => BooleanSchema(p.a)
          case "object"  => ObjectSchema(p.a)
          case "null"    => NullSchema(p.a)
          case "array"   => ArraySchema(p.a)
          case other =>
            throw new IllegalStateException(
              s"Invalid type name, expected one of [null, boolean, object, array, number, integer, string], but got $other")
        }
      case other =>
        throw new IllegalStateException(s"Invalid type definition, expected an array of strings, but got $other")

    }
    if (variants.isEmpty) throw new IllegalStateException(s"")
    else if (variants.size == 1) variants.head
    else OneOfAnyOfSchema(p.a, variants, isOneOf = true)
  }

  def attemptReadReference(p: Parameters): Option[Schema] =
    (p.json \ "$ref")
      .asOpt[String]
      .map { reference =>
        resolveReference(p, reference)
      }

  def resolveReference(p: Parameters, reference: String): Schema = {

    val uri: URI = p.referenceResolver.resolveUri(URI.create(reference))

    val path2 = p.referenceResolver.uriToPath(uri)

    p.referenceResolver
      .lookupSchema(uri, readSchema(_, path2, _, externalDescription = p.description, p.requiredFields, _)) match {

      case Some(referencedSchema) =>
        if (p.referenceResolver.isInternal(referencedSchema.uri)) {
          if (referencedSchema.primitive)
            SchemaUtils.copy(referencedSchema, p.name, "$ref" :: p.path, p.description)
          else
            InternalSchemaReference(p.a, reference, referencedSchema, p.requiredFields)
        } else
          ExternalSchemaReference(p.a, reference = reference, referencedSchema, p.requiredFields)

      case None =>
        throw new IllegalStateException(s"Cannot resolve schema reference $reference when at ${p.currentUri}")
    }
  }

  def attemptReadOneOf(p: Parameters): Option[Schema] = {
    val (requiredFields, alternatives) = readRequired(p.json)
    (p.json \ "oneOf")
      .asOpt[JsArray]
      .map { array =>
        readOneOfAnyOfSchema(
          p.copy(path = "oneOf" :: p.path, requiredFields = p.requiredFields ++ requiredFields),
          array,
          alternatives,
          isOneOf = true)
      }
  }

  def attemptReadAnyOf(p: Parameters): Option[Schema] = {
    val (requiredFields, alternatives) = readRequired(p.json)
    (p.json \ "anyOf")
      .asOpt[JsArray]
      .map { array =>
        readOneOfAnyOfSchema(
          p.copy(path = "anyOf" :: p.path, requiredFields = p.requiredFields ++ requiredFields),
          array,
          alternatives,
          isOneOf = false)
      }
  }

  def readOneOfAnyOfSchema(
    p: Parameters,
    array: JsArray,
    alternativeRequiredFields: Seq[Set[String]],
    isOneOf: Boolean): OneOfAnyOfSchema = {
    val variants = array.value.zipWithIndex.map {
      case (jsObject: JsObject, i) =>
        readSchema(
          p.name,
          i.toString :: p.path,
          jsObject,
          requiredFields = p.requiredFields,
          currentReferenceResolver = p.referenceResolver)

      case (other, i) =>
        throw new IllegalStateException(s"Invalid ${if (isOneOf) "oneOf" else "anyOf"} schema, expected ${p.path.reverse
          .mkString("/")}[$i] to be an object, but got ${other.getClass.getSimpleName}.")
    }
    OneOfAnyOfSchema(p.a, variants, alternativeRequiredFields, isOneOf)
  }

  def attemptReadAllOf(p: Parameters): Option[Schema] =
    (p.json \ "allOf")
      .asOpt[JsArray]
      .map { array =>
        array.value.length match {
          case 0 =>
            throw new IllegalStateException("Invalid schema, allOf array must not be empty.")
          case 1 =>
            array.value.head match {
              case json: JsObject =>
                readSchema(p.name, "0" :: "allOf" :: p.path, json, p.description, p.requiredFields, p.referenceResolver)
              case other =>
                throw new IllegalStateException(
                  s"Invalid schema, allOf array element must be valid schema object, but got $other")
            }
          case many =>
            val mergedJson = array.value.foldLeft(Json.obj())((a, v) =>
              v match {
                case json: JsObject => {
                  val dereferenced: JsObject = deepDereference(json, p.referenceResolver)
                  a.deepMerge(dereferenced)
                }
                case other =>
                  throw new IllegalStateException(
                    s"Invalid schema, allOf array element must be valid schema object, but got $other")
            })
            readSchema(p.name, "allOf" :: p.path, mergedJson, p.description, p.requiredFields, p.referenceResolver)
        }
      }

  def attemptReadNot(p: Parameters): Option[Schema] =
    (p.json \ "not")
      .asOpt[JsObject]
      .map { json =>
        val schema: Schema =
          readSchema(p.name, "not" :: p.path, json, p.description, p.requiredFields, p.referenceResolver)
        NotSchema(p.a, schema)
      }

  def attemptReadIfThenElse(p: Parameters): Option[Schema] =
    (p.json \ "if")
      .asOpt[JsObject]
      .flatMap { ifJson =>
        (p.json \ "then")
          .asOpt[JsObject]
          .map { thenJson =>
            val condition: Schema =
              readSchema(p.name, "if" :: p.path, ifJson, p.description, p.requiredFields, p.referenceResolver)

            val schema: Schema =
              readSchema(p.name, "then" :: p.path, thenJson, p.description, p.requiredFields, p.referenceResolver)

            val elseSchema: Option[Schema] = (p.json \ "else")
              .asOpt[JsObject]
              .map { elseJson =>
                readSchema(p.name, "else" :: p.path, elseJson, p.description, p.requiredFields, p.referenceResolver)
              }

            IfThenElseSchema(p.a, condition, schema, elseSchema)
          }
      }

  def deepDereference(json: JsObject, referenceResolver: SchemaReferenceResolver): JsObject =
    (json \ "$ref")
      .asOpt[String]
      .map(ref => referenceResolver.resolveUri(URI.create(ref)))
      .flatMap(referenceResolver.lookupJson)
      .map {
        case (json2: JsObject, resolver) =>
          deepDereference(json2, resolver)

        case other =>
          throw new IllegalStateException(
            s"Invalid schema, expected $$ref target to be a valid schema object, but got $other")
      }
      .getOrElse(json)

  def readObjectSchema(p: Parameters): Schema = {

    val (required, alternatives) = readRequired(p.json)
    val p2 = p.copy(requiredFields = required)

    val propertiesOpt: Option[Seq[Schema]] =
      attemptReadProperties(p2)
        .flatMap(emptyAsNone)

    val patternPropertiesOpt: Option[Seq[Schema]] =
      attemptReadPatternProperties(p2)
        .flatMap(emptyAsNone)

    val additionalPropertiesOpt: Option[Seq[Schema]] =
      attemptReadAdditionalProperties(p2)
        .flatMap(emptyAsNone)

    (propertiesOpt, patternPropertiesOpt, additionalPropertiesOpt) match {

      case (None, Some(patternProperties), None) =>
        MapSchema(p.a, patternProperties, required, alternatives)

      case _ =>
        ObjectSchema(
          attributes = p.a,
          properties =
            propertiesOpt.getOrElse(Seq.empty) ++
              additionalPropertiesOpt.getOrElse(Seq.empty),
          requiredFields = required,
          alternativeRequiredFields = alternatives,
          patternProperties = patternPropertiesOpt
        )
    }
  }

  def attemptReadProperties(p: Parameters): Option[Seq[Schema]] =
    (p.json \ "properties")
      .asOpt[JsObject]
      .map(properties =>
        properties.fields
          .map(_._1)
          .distinct
          .map(name => {
            (properties \ name).as[JsValue] match {
              case fieldProperty: JsObject =>
                readSchema(
                  name = name,
                  currentPath = name :: "properties" :: p.path,
                  value = fieldProperty,
                  requiredFields = p.requiredFields,
                  currentReferenceResolver = p.referenceResolver)

              case boolean: JsBoolean =>
                readSchema(
                  name = name,
                  currentPath = name :: "properties" :: p.path,
                  value = boolean,
                  requiredFields = p.requiredFields,
                  currentReferenceResolver = p.referenceResolver)

              case other =>
                throw new IllegalStateException(
                  s"Invalid object schema, expected property '$name' to be an object or boolean, but got ${other.getClass.getSimpleName} at ${p.currentUri}")
            }
          }))

  def attemptReadPatternProperties(p: Parameters): Option[Seq[Schema]] =
    (p.json \ "patternProperties")
      .asOpt[JsObject]
      .map(patternProperties =>
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
                    value = fieldProperty,
                    requiredFields = p.requiredFields,
                    currentReferenceResolver = p.referenceResolver
                  )

                case boolean: JsBoolean =>
                  readSchema(
                    name = pattern,
                    currentPath = pattern :: "patternProperties" :: p.path,
                    value = boolean,
                    requiredFields = p.requiredFields,
                    currentReferenceResolver = p.referenceResolver
                  )

                case other =>
                  throw new IllegalStateException(
                    s"Invalid object schema, expected '$pattern' to be an object, but got ${other.getClass.getSimpleName} at ${p.currentUri}")
              }
            }
        })

  def attemptReadAdditionalProperties(p: Parameters): Option[Seq[Schema]] =
    (p.json \ "additionalProperties")
      .asOpt[JsValue] match {
      case Some(property: JsObject) =>
        Some(
          readSchema(
            name = p.name,
            currentPath = "additionalProperties" :: p.path,
            value = property,
            requiredFields = p.requiredFields,
            currentReferenceResolver = p.referenceResolver))
          .map {
            case o: ObjectSchema => o.properties
            case m: MapSchema    => m.patternProperties
            case other           => Seq(other)
          }

      case _ => None
    }

  def readArraySchema(p: Parameters): ArraySchema = {

    val minItems = (p.json \ "minItems").asOpt[Int]
    val maxItems = (p.json \ "maxItems").asOpt[Int]
    val uniqueItems = (p.json \ "uniqueItems").asOpt[Boolean]
    val minContains = (p.json \ "minContains").asOpt[Int]
    val maxContains = (p.json \ "maxContains").asOpt[Int]

    /*
      9.3.1.1. items
      The value of "items" MUST be either a valid JSON Schema or an array of valid JSON Schemas.
     */
    val items: Option[Seq[Schema]] = (p.json \ "items")
      .asOpt[JsValue]
      .flatMap({
        case itemSchema: JsObject =>
          Some(
            Seq(
              readSchema(
                NameUtils.singular(p.name),
                "items" :: p.path,
                itemSchema,
                requiredFields = Seq(p.name),
                currentReferenceResolver = p.referenceResolver)))

        case array: JsArray =>
          array.value.toList match {
            case Nil => None
            case many =>
              Some(many map {
                case json: JsObject =>
                  readSchema(
                    NameUtils.singular(p.name),
                    "0" :: "items" :: p.path,
                    json,
                    requiredFields = Seq(p.name),
                    currentReferenceResolver = p.referenceResolver)

                case other =>
                  throw new IllegalStateException(
                    s"Invalid schema, expected array schema item to be an object, but got $other at ${p.currentUri}")
              })
          }

        case other =>
          throw new IllegalStateException(
            s"Invalid schema, expected object or an array, but got $other at ${p.currentUri}")
      })

    ArraySchema(p.a, items, minItems, maxItems, uniqueItems, minContains, maxContains)
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
              val uri: URI = {
                val reference = SchemaReferenceResolver.pathToReference(name :: propertyName :: path)
                referenceResolver.resolveUri(URI.create(reference))
              }
              val path2 = referenceResolver.uriToPath(uri)
              referenceResolver
                .lookupSchema(uri, readSchema(_, path2, _, description, Seq.empty, _))
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

  def readStringSchema(p: Parameters): StringSchema = {

    val pattern = (p.json \ "pattern").asOpt[String]
    val minLength = (p.json \ "minLength").asOpt[Int]
    val maxLength = (p.json \ "maxLength").asOpt[Int]
    val enum = readEnum[String](p)

    StringSchema(p.a, pattern, minLength, maxLength, enum)
  }

  def readEnum[T: Reads](p: Parameters): Option[Seq[T]] =
    (p.json \ "enum")
      .asOpt[Seq[T]]
      .orElse(
        (p.json \ "const").asOpt[T].map(Seq(_))
      )

  def readNumberSchema(p: Parameters): NumberSchema = {

    val minimum = (p.json \ "minimum").asOpt[BigDecimal]
    val maximum = (p.json \ "maximum").asOpt[BigDecimal]
    val exclusiveMinimum = (p.json \ "exclusiveMinimum").asOpt[BigDecimal]
    val exclusiveMaximum = (p.json \ "exclusiveMaximum").asOpt[BigDecimal]
    val multipleOf = (p.json \ "multipleOf").asOpt[BigDecimal]
    val enum = readEnum[BigDecimal](p)

    NumberSchema(p.a, minimum, maximum, exclusiveMinimum, exclusiveMaximum, multipleOf, enum)
  }

  def readIntegerSchema(p: Parameters): IntegerSchema = {

    val minimum = (p.json \ "minimum").asOpt[Int]
    val maximum = (p.json \ "maximum").asOpt[Int]
    val exclusiveMinimum = (p.json \ "exclusiveMinimum").asOpt[Int]
    val exclusiveMaximum = (p.json \ "exclusiveMaximum").asOpt[Int]
    val multipleOf = (p.json \ "multipleOf").asOpt[Int]
    val enum = readEnum[Int](p)

    IntegerSchema(p.a, minimum, maximum, exclusiveMinimum, exclusiveMaximum, multipleOf, enum)
  }

  def readBooleanSchema(p: Parameters): BooleanSchema = {

    val enum = readEnum[Boolean](p)

    BooleanSchema(p.a, enum)
  }

  final val implicitReaders: Seq[(Set[String], Parameters => Schema)] = Seq(
    Vocabulary.objectCoreVocabulary       -> readObjectSchema,
    Vocabulary.objectValidationVocabulary -> readObjectSchema,
    Vocabulary.stringValidationVocabulary -> readStringSchema,
    Vocabulary.numberValidationVocabulary -> readNumberSchema,
    Vocabulary.arrayCoreVocabulary        -> readArraySchema,
    Vocabulary.arrayValidationVocabulary  -> readArraySchema
  )

  def attemptReadImplicitType(p: Parameters): Option[Schema] = {
    import Vocabulary._

    val fields: Seq[String] = p.json.fields.map(_._1)

    implicitReaders
      .foldLeft[Option[Schema]](None) {
        case (a, (v, r)) => a.orElse(if (isKeywordIn(v)(fields)) Some(r(p)) else None)
      }
      .orElse {
        attemptReadEnumOrConst(p)
      }
  }

  def attemptReadEnumOrConst(p: Parameters): Option[Schema] =
    (p.json \ "enum")
      .asOpt[Seq[JsValue]]
      .orElse(
        (p.json \ "const").asOpt[JsValue].map(Seq(_))
      )
      .flatMap(emptyAsNone)
      .map { enum =>
        val types = enum.groupBy(_.getClass.getSimpleName).values.toSeq
        types.size match {
          case 1 =>
            enum.head match {
              case JsNull       => NullSchema(p.a)
              case _: JsString  => StringSchema(p.a, enum = Some(enum.map(_.as[String])))
              case _: JsNumber  => NumberSchema(p.a, enum = Some(enum.map(_.as[BigDecimal])))
              case _: JsBoolean => BooleanSchema(p.a, enum = Some(enum.map(_.as[Boolean])))
              case _: JsArray   => ArraySchema(p.a)
              case other =>
                throw new IllegalStateException(s"Unsupported feature, enum of [$other] at ${p.currentUri}")
            }
          case _ =>
            val variants = types.map {
              case (JsNull :: _)              => NullSchema(p.a)
              case xs @ ((_: JsString) :: _)  => StringSchema(p.a, enum = Some(xs.map(_.as[String])))
              case xs @ ((_: JsNumber) :: _)  => NumberSchema(p.a, enum = Some(xs.map(_.as[BigDecimal])))
              case xs @ ((_: JsBoolean) :: _) => BooleanSchema(p.a, enum = Some(xs.map(_.as[Boolean])))
              case xs @ ((_: JsArray) :: _) =>
                throw new IllegalStateException(
                  s"Unsupported feature, enum of array of arrays ${xs.mkString("[", ",", "]")} at ${p.currentUri}")
              case other =>
                throw new IllegalStateException(
                  s"Unsupported feature, enum of ${other.mkString("[", ",", "]")} at ${p.currentUri}")
            }
            OneOfAnyOfSchema(p.a, variants, Seq.empty, isOneOf = true)
        }
      }

  def decideResolverAndPath(
    name: String,
    path: List[String],
    json: JsObject,
    referenceResolver: SchemaReferenceResolver,
    id: Option[URI]): (SchemaReferenceResolver, List[String]) =
    id.map { uri =>
        val uri2 = if (uri.isAbsolute) uri else referenceResolver.resolveUri(uri)
        (CachingReferenceResolver(uri2, name, json, Some(referenceResolver)), SchemaReferenceResolver.rootPath(uri))
      }
      .getOrElse((referenceResolver, path))

  def emptyAsNone[T](seq: Seq[T]): Option[Seq[T]] = if (seq.isEmpty) None else Some(seq)
}
