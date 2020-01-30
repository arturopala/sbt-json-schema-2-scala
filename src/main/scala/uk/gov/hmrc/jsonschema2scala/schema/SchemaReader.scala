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

import java.net.URI

import play.api.libs.json._
import uk.gov.hmrc.jsonschema2scala.utils.JsonUtils
import uk.gov.hmrc.jsonschema2scala.utils.OptionOps.{defined, emptyAsNone}
import uk.gov.hmrc.jsonschema2scala.utils.SeqOps.{hasSingleItem, isEmpty}
import Keywords.{`$defs`, allOf, anyOf, oneOf}

import scala.util.Try

object SchemaReader {

  def read(schemaSource: SchemaSource, resolver: SchemaReferenceResolver, debug: DebugOptions): Schema = {
    val path = SchemaReferenceResolver.rootPath(schemaSource.uri)
    resolver.lookupSchema(
      schemaSource.uri,
      Some(
        (_, jsValue, _) =>
          readSchema(
            name = schemaSource.name,
            currentPath = path,
            value = jsValue,
            externalDescription = None,
            requiredFields = Seq.empty,
            currentReferenceResolver = resolver,
            processDefinitions = true,
            debug = debug
        ))
    ) match {
      case None =>
        throw new IllegalStateException(s"Unexpected error, schema lookup failed for ${schemaSource.uri}")

      case Some((schema, _)) => schema
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
    referenceResolver: SchemaReferenceResolver,
    processDefinitions: Boolean,
    debug: DebugOptions) {

    val a: SchemaAttributes =
      SchemaAttributes(name, path, description, definitions, required, custom)

    def currentUri: String = SchemaReferenceResolver.pathToReference(path)
  }

  val keywordsNotInVocabulary: Seq[(String, JsValue)] => Set[String] =
    Vocabulary.keywordsNotIn(Vocabulary.allKeywords)

  val keywordsInVocabularyNotMeta: Seq[(String, JsValue)] => Set[String] =
    Vocabulary.keywordsIn(Vocabulary.allKeywordsButMeta)

  def readSchema(p: Parameters): Schema =
    readSchema(
      p.name,
      p.path,
      p.json,
      p.description,
      p.requiredFields,
      p.referenceResolver,
      p.processDefinitions,
      p.debug)

  def readSchema(
    name: String,
    currentPath: List[String],
    value: JsValue,
    externalDescription: Option[String] = None,
    requiredFields: Seq[String],
    currentReferenceResolver: SchemaReferenceResolver,
    processDefinitions: Boolean,
    debug: DebugOptions): Schema =
    value match {

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
          resolverAndPath(name, currentPath, json, currentReferenceResolver, id)

        if (debug.enabled && debug.traceReadingProgress) {
          println(s"> ${debug.incIndent(".")}${SchemaReferenceResolver.pathToReference(path)}")
        }

        val description: Option[String] = externalDescription.orElse(attemptReadDescription(json))

        val definitions: Seq[Schema] =
          if (processDefinitions)
            readDefinitions("definitions", name, path, json, description, referenceResolver, debug) ++
              readDefinitions("$defs", name, path, json, description, referenceResolver, debug)
          else Seq.empty

        val custom: Option[Map[String, JsValue]] = {
          val set = keywordsNotInVocabulary(json.fields)
          if (set.isEmpty) None else Some(set.map(k => (k, (json \ k).as[JsValue])).toMap)
        }

        val p =
          Parameters(
            name,
            path,
            description,
            definitions,
            isMandatory,
            custom,
            json,
            requiredFields,
            referenceResolver,
            processDefinitions,
            debug
          )

        val schema = attemptReadExplicitType(p)
          .orElse { attemptReadReference(p) }
          .orElse { attemptReadImplicitType(p) }
          .orElse { attemptReadOneOf(p) }
          .orElse { attemptReadAnyOf(p) }
          .orElse { attemptReadAllOf(p) }
          .orElse { attemptReadNot(p) }
          .orElse { attemptReadIfThenElse(p) }
          .getOrElse {
            // fallback to an empty object schema
            ObjectSchema(
              attributes = p.a,
              requiredFields = p.requiredFields
            )
          }

        if (debug.enabled && debug.traceReadingProgress) {
          println(s"< ${debug.decIndent("-")}${schema.info}")
        }

        schema

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
        case JsString(schemaType) => readSchemaWithType(p, schemaType)

        case JsArray(schemaTypeArray) =>
          attemptReadOneOf(p)
            .orElse(attemptReadAnyOf(p))
            .getOrElse(readSchemaWithTypesArray(p, schemaTypeArray))

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
      case "null"    => NullSchema(p.a)
      case other =>
        throw new IllegalStateException(
          s"Invalid type name, expected one of [null, boolean, object, array, number, integer, string], but got $other")
    }

  def readSchemaWithTypesArray(p: Parameters, schemaTypeArray: Seq[JsValue]): Schema = {

    val variants: Seq[Schema] = schemaTypeArray.distinct.map {
      case JsString(valueType) =>
        valueType match {
          case "string"  => readStringSchema(p)
          case "number"  => readNumberSchema(p)
          case "integer" => readIntegerSchema(p)
          case "boolean" => readBooleanSchema(p)
          case "object"  => readObjectSchema(p)
          case "array"   => readArraySchema(p)
          case "null"    => NullSchema(p.a)
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
      .map(readReference(p, _))

  def readReference(p: Parameters, reference: String): Schema = {

    val uri: URI = p.referenceResolver.resolveUri(URI.create(reference))
    val path2 = p.referenceResolver.uriToPath(uri)

    if (p.debug.enabled && p.debug.traceReadingProgress) {
      println(s"R ${p.debug.curIndent(">")}$uri")
    }

    p.referenceResolver
      .lookupSchema(
        uri,
        Some(
          (name, value, resolver) =>
            readSchema(
              name,
              path2,
              value,
              externalDescription = p.description,
              p.requiredFields,
              resolver,
              processDefinitions = true,
              p.debug))
      ) match {

      case Some((referencedSchema, resolver)) =>
        if (p.referenceResolver.isInternal(referencedSchema.uri)) {
          if (referencedSchema.primitive)
            SchemaUtils.copy(referencedSchema, p.name, "$ref" :: p.path, p.description)
          else {
            InternalSchemaReference(
              attributes = p.a,
              reference = uri.toString,
              schema = referencedSchema,
              requiredFields = p.requiredFields)
          }
        } else {
          val rootSchema: Option[Schema] =
            if (referencedSchema.primitive) None
            else
              resolver.rootSchemaSource
                .map(_.uri)
                .flatMap(
                  uri =>
                    resolver.lookupSchema(
                      uri,
                      Some((name, value, resolver) =>
                        readSchema(name, Nil, value, None, Seq.empty, resolver, processDefinitions = true, p.debug))))
                .map(_._1)

          ExternalSchemaReference(
            attributes = p.a,
            reference = uri.toString,
            schema = referencedSchema,
            requiredFields = p.requiredFields,
            rootSchema = rootSchema
          )
        }

      case None =>
        throw new IllegalStateException(
          s"Cannot resolve schema reference $reference (resolved as $uri) at ${p.referenceResolver}")
    }
  }

  final val implicitReaders: Seq[(Set[String], Parameters => Schema)] = Seq(
    Vocabulary.objectVocabulary           -> readObjectSchema,
    Vocabulary.stringValidationVocabulary -> readStringSchema,
    Vocabulary.numberValidationVocabulary -> readNumberSchema,
    Vocabulary.arrayVocabulary            -> readArraySchema
  )

  def attemptReadImplicitType(p: Parameters): Option[Schema] = {
    import Vocabulary._

    val fields: Seq[String] = p.json.fields.map(_._1)

    implicitReaders
      .foldLeft[Option[Schema]](None) {
        case (a, (v, r)) => a.orElse(if (isKeywordIn(v)(fields)) Some(r(p)) else None)
      }
      .filterNot(SchemaUtils.isEmptySchema)
      .orElse {
        attemptReadEnumOrConst(p)
      }
  }

  def readObjectSchema(p: Parameters): Schema = {

    val (required, alternatives) = readRequired(p.json)
    val p2 = p.copy(requiredFields = required)

    val hasProperties = SchemaUtils.checkKeyExistsAndNonEmpty(p2.json, "properties")
    val hasPatternProperties = SchemaUtils.checkKeyExistsAndNonEmpty(p2.json, "patternProperties")

    val extras: Map[String, Seq[JsObject]] =
      readSchemaFields(p2, Set(oneOf, anyOf, allOf, "additionalProperties"), Vocabulary.schemaKeyVocabulary)

    val schema = if (extras.nonEmpty) {
      if (!hasProperties && !hasPatternProperties && extras.size == 1) {
        extras.head match {
          case ("additionalProperties", hasSingleItem(json)) =>
            val s = readSchema(p2.copy(json = json, path = "additionalProperties" :: p.path))
            s.withDefinitions(s.definitions ++ p2.definitions)

          case (name, hasSingleItem(json)) =>
            val s = readSchema(p2.copy(json = json, path = "0" :: name :: p.path))
            s.withDefinitions(s.definitions ++ p2.definitions)

          case _ =>
            readObjectSchemaWithExtras(p2, extras)
        }
      } else
        readObjectSchemaWithExtras(p2, extras)
    } else {

      val patternPropertiesOpt: Option[Seq[Schema]] =
        attemptReadPatternProperties(p2).flatMap(emptyAsNone)

      if (!hasProperties && hasPatternProperties) {
        MapSchema(p2.a, patternPropertiesOpt.get, required, alternatives)
      } else {

        val propertiesOpt: Option[Seq[Schema]] =
          attemptReadProperties(p2).flatMap(emptyAsNone)

        ObjectSchema(
          attributes = p2.a,
          properties = propertiesOpt.getOrElse(Seq.empty),
          requiredFields = required,
          alternativeRequiredFields = alternatives,
          patternProperties = patternPropertiesOpt
        )
      }
    }
    schema
  }

  def readSchemaFields(
    p: Parameters,
    keys: Set[String],
    requiredObjectFields: Set[String]): Map[String, Seq[JsObject]] =
    keys.toSeq
      .map(key =>
        emptyAsNone(p.json.fields.collect {
          case (`key`, jsArray: JsArray) =>
            jsArray.value.collect {
              case jsObject: JsObject if jsObject.keys.intersect(requiredObjectFields).nonEmpty => jsObject
            }
          case (`key`, jsObject: JsObject) if jsObject.keys.intersect(requiredObjectFields).nonEmpty => Seq(jsObject)
        }.flatten).map((key, _)))
      .collect(defined)
      .groupBy(_._1)
      .mapValues(_.flatMap(_._2))

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
                  currentReferenceResolver = p.referenceResolver,
                  processDefinitions = p.processDefinitions,
                  debug = p.debug
                )

              case boolean: JsBoolean =>
                readSchema(
                  name = name,
                  currentPath = name :: "properties" :: p.path,
                  value = boolean,
                  requiredFields = p.requiredFields,
                  currentReferenceResolver = p.referenceResolver,
                  processDefinitions = p.processDefinitions,
                  debug = p.debug
                )

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
                    currentReferenceResolver = p.referenceResolver,
                    processDefinitions = p.processDefinitions,
                    debug = p.debug
                  )

                case boolean: JsBoolean =>
                  readSchema(
                    name = pattern,
                    currentPath = pattern :: "patternProperties" :: p.path,
                    value = boolean,
                    requiredFields = p.requiredFields,
                    currentReferenceResolver = p.referenceResolver,
                    processDefinitions = p.processDefinitions,
                    debug = p.debug
                  )

                case other =>
                  throw new IllegalStateException(
                    s"Invalid object schema, expected '$pattern' to be an object, but got ${other.getClass.getSimpleName} at ${p.currentUri}")
              }
            }
        })

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
            Seq(readSchema(
              NameUtils.singular(p.name),
              "items" :: p.path,
              itemSchema,
              requiredFields = Seq(p.name),
              currentReferenceResolver = p.referenceResolver,
              processDefinitions = p.processDefinitions,
              debug = p.debug
            )))

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
                    currentReferenceResolver = p.referenceResolver,
                    processDefinitions = p.processDefinitions,
                    debug = p.debug
                  )

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
    referenceResolver: SchemaReferenceResolver,
    debug: DebugOptions): Seq[Schema] =
    (json \ propertyName)
      .asOpt[JsObject]
      .map { properties =>
        if (debug.enabled && debug.traceReadingProgress) {
          println(s"D ${debug.curIndent("&")}${properties.fields.map(_._1).distinct.mkString(",")}")
        }
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
              .lookupSchema(
                uri,
                Some((name, value, resolver) =>
                  readSchema(name, path2, value, description, Seq.empty, resolver, processDefinitions = true, debug)))
              .map(_._1)
          })
          .collect { case Some(x) => x }
      }
      .getOrElse(Seq.empty)

  def readRequired(json: JsObject): (Seq[String], Seq[Set[String]]) = {
    val required: Seq[String] = (json \ "required").asOpt[Seq[String]].getOrElse(Seq.empty)
    val (required2, alternatives) = (json \ oneOf)
      .asOpt[Seq[JsObject]]
      .map(s => {
        val names = s.map(o => (o \ "required").asOpt[Set[String]].getOrElse(Set.empty))
        val required = names.reduce(_ intersect _)
        val variants = names.map(_ -- required)
        (required.toSeq, variants)
      })
      .getOrElse((Seq.empty, Seq.empty))

    (required ++ required2, alternatives)
  }

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

  def attemptReadOneOf(p: Parameters): Option[Schema] = {
    val (requiredFields, alternatives) = readRequired(p.json)
    (p.json \ oneOf)
      .asOpt[JsArray]
      .flatMap { array =>
        readOneOfAnyOfSchema(
          p.copy(path = p.path, requiredFields = p.requiredFields ++ requiredFields),
          array,
          alternatives,
          isOneOf = true)
      }
  }

  def attemptReadAnyOf(p: Parameters): Option[Schema] = {
    val (requiredFields, alternatives) = readRequired(p.json)
    (p.json \ anyOf)
      .asOpt[JsArray]
      .flatMap { array =>
        readOneOfAnyOfSchema(
          p.copy(path = p.path, requiredFields = p.requiredFields ++ requiredFields),
          array,
          alternatives,
          isOneOf = false)
      }
  }

  def readOneOfAnyOfSchema(
    p: Parameters,
    array: JsArray,
    alternativeRequiredFields: Seq[Set[String]],
    isOneOf: Boolean): Option[Schema] = {

    def readVariant(path: List[String]): JsValue => (Seq[Schema], Seq[Schema]) = {
      case jsObject: JsObject =>
        readSchema(
          p.name,
          path,
          jsObject,
          requiredFields = p.requiredFields,
          currentReferenceResolver = p.referenceResolver,
          processDefinitions = p.processDefinitions,
          debug = p.debug
        ) match {
          case s: OneOfAnyOfSchema => (s.variants, s.definitions)
          case s                   => (Seq(s), Seq.empty)
        }

      case jsArray: JsArray =>
        jsArray.value.zipWithIndex.foldLeft((Seq.empty[Schema], Seq.empty[Schema])) {
          case ((v, d), (s, i)) =>
            val (v1, d1) = readVariant(i.toString :: p.path)(s)
            (v ++ v1, d ++ d1)
        }

      case other =>
        throw new IllegalStateException(s"Invalid ${if (isOneOf) oneOf else anyOf} schema, expected ${p.path.reverse
          .mkString("/")} to be an object or array, but got ${other.getClass.getSimpleName}.")
    }

    val (variants, definitions) = array.value.zipWithIndex
      .foldLeft((Seq.empty[Schema], Seq.empty[Schema])) {
        case ((v, d), (s, i)) =>
          val (v1, d1) = readVariant(i.toString :: (if (isOneOf) oneOf else anyOf) :: p.path)(s)
          (v ++ v1, d ++ d1)
      }

    val variants2 = variants.filterNot(SchemaUtils.isEmptySchema)

    if (variants2.isEmpty) None
    else
      Some(
        OneOfAnyOfSchema(p.a, variants2, alternativeRequiredFields, isOneOf)
          .withDefinitions(p.definitions ++ definitions))
  }

  val keywordsToRemoveWhenMergingSchema: Set[String] =
    Set(Keywords.definitions, `$defs`, Keywords.`$id`)

  def attemptReadAllOf(p: Parameters): Option[Schema] =
    (p.json \ allOf)
      .asOpt[JsArray]
      .map(_.value)
      .map {
        case isEmpty() =>
          throw new IllegalStateException("Invalid schema, allOf array must not be empty.")

        case hasSingleItem(jsValue) =>
          jsValue match {
            case jsObject: JsObject =>
              val schema = readSchema(
                p.name,
                "0" :: allOf :: p.path,
                jsObject,
                requiredFields = p.requiredFields,
                currentReferenceResolver = p.referenceResolver,
                processDefinitions = p.processDefinitions,
                debug = p.debug
              )
              AllOfSchema(p.a, Seq.empty, p.requiredFields, schema)

            case other =>
              throw new IllegalStateException(s"Invalid allOf schema, expected ${p.path.reverse
                .mkString("/")}/0 to be an object, but got ${other.getClass.getSimpleName}.")
          }

        case arrayValues => {
          val parts: Seq[Schema] = arrayValues.zipWithIndex.map {
            case (jsObject: JsObject, i) =>
              readSchema(
                p.name,
                i.toString :: allOf :: p.path,
                jsObject,
                requiredFields = p.requiredFields,
                currentReferenceResolver = p.referenceResolver,
                processDefinitions = p.processDefinitions,
                debug = p.debug
              )

            case (other, i) =>
              throw new IllegalStateException(s"Invalid allOf schema, expected ${p.path.reverse
                .mkString("/")}/$i to be an object, but got ${other.getClass.getSimpleName}.")
          }

          val (mergedJson, definitions, resolver) =
            mergePartialSchemas(p.copy(path = allOf :: p.path), arrayValues.collect {
              case jsObject: JsObject => jsObject
            })

          val aggregatedSchema: Schema = readSchema(
            p.name,
            p.path,
            mergedJson,
            p.description,
            p.requiredFields,
            resolver,
            processDefinitions = false,
            debug = p.debug)

          AllOfSchema(p.a, parts, p.requiredFields, aggregatedSchema.withDefinitions(Seq.empty))
            .withDefinitions(p.definitions ++ definitions /* ++ aggregatedSchema.attributes.definitions*/ )
        }
      }

  def attemptReadNot(p: Parameters): Option[Schema] =
    (p.json \ "not")
      .asOpt[JsObject]
      .map { json =>
        val schema: Schema =
          readSchema(
            p.name,
            "not" :: p.path,
            json,
            p.description,
            p.requiredFields,
            p.referenceResolver,
            p.processDefinitions,
            debug = p.debug)
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
              readSchema(
                p.name,
                "if" :: p.path,
                ifJson,
                p.description,
                p.requiredFields,
                p.referenceResolver,
                p.processDefinitions,
                debug = p.debug)

            val schema: Schema =
              readSchema(
                p.name,
                "then" :: p.path,
                thenJson,
                p.description,
                p.requiredFields,
                p.referenceResolver,
                p.processDefinitions,
                debug = p.debug)

            val elseSchema: Option[Schema] = (p.json \ "else")
              .asOpt[JsObject]
              .map { elseJson =>
                readSchema(
                  p.name,
                  "else" :: p.path,
                  elseJson,
                  p.description,
                  p.requiredFields,
                  p.referenceResolver,
                  p.processDefinitions,
                  debug = p.debug)
              }

            IfThenElseSchema(p.a, condition, schema, elseSchema)
          }
      }

  def resolverAndPath(
    name: String,
    path: List[String],
    json: JsObject,
    referenceResolver: SchemaReferenceResolver,
    id: Option[URI]): (SchemaReferenceResolver, List[String]) =
    id.map { uri =>
        val uri2 = if (uri.isAbsolute) uri else referenceResolver.resolveUri(uri)
        (SchemaReferenceResolver(uri2, name, json, Some(referenceResolver)), SchemaReferenceResolver.rootPath(uri))
      }
      .getOrElse((referenceResolver, path))

  def readObjectSchemaWithExtras(p: Parameters, extras: Map[String, Seq[JsObject]]): Schema = {

    val coreObjectJson =
      SchemaUtils.deepResolveReferences(
        p.json - allOf - oneOf - anyOf - "additionalProperties" - Keywords.definitions - `$defs`,
        p.referenceResolver)

    val (stage0Json, stage0Definitions, stage0Resolver) = extras
      .get("additionalProperties")
      .flatMap {
        case hasSingleItem(additional) =>
          Some(preparePartialSchemaToMerge(p.copy(json = additional, path = "additionalProperties" :: p.path)))
        case _ =>
          None
      }
      .map {
        case (json, ds, rs) => (JsonUtils.deepMerge(coreObjectJson, json).as[JsObject], ds, SchemaReferenceResolver(rs))
      }
      .getOrElse((coreObjectJson, Seq.empty, p.referenceResolver))

    val (stage1Json, stage1Definitions, stage1Resolver) = extras
      .get(allOf)
      .map { partials =>
        val (aggregatedJson, definitions, resolver) = mergePartialSchemas(p.copy(path = allOf :: p.path), partials)
        val mergedJson = JsonUtils.deepMerge(stage0Json, aggregatedJson).as[JsObject]
        (mergedJson, definitions, resolver)
      }
      .getOrElse((stage0Json, Seq.empty, p.referenceResolver))

    val (stage2Json, stage2Definitions) = (extras.get(oneOf), extras.get(anyOf)) match {
      case (None, None) => (stage1Json, Seq.empty)
      case (Some(variants), None) =>
        copyAndMergeBaseIntoVariantSchemas(p.copy(json = stage1Json), oneOf, variants)
      case (None, Some(variants)) =>
        copyAndMergeBaseIntoVariantSchemas(p.copy(json = stage1Json), anyOf, variants)
      case (Some(variants1), Some(variants2)) =>
        ??? //FIXME
    }

    val schema: Schema = readSchema(
      p.copy(json = stage2Json, referenceResolver = SchemaReferenceResolver(Seq(stage0Resolver, stage1Resolver))))

    schema.withDefinitions(
      p.definitions ++ stage0Definitions ++ stage1Definitions ++ stage2Definitions ++ schema.attributes.definitions)
  }

  def copyAndMergeBaseIntoVariantSchemas(
    p: Parameters,
    conditionalKeyword: String,
    variants: Seq[JsObject]): (JsObject, Seq[Schema]) = {

    val preservedVocabulary = Vocabulary.metaCoreVocabulary - Keywords.definitions - `$defs`
    val preservedJson = JsonUtils.filterObjectFields(p.json)(preservedVocabulary.contains)
    val relocatingJson = JsonUtils.filterObjectFields(p.json)(Vocabulary.objectVocabulary.contains)

    val (relocatingPrunedJson, promotedToDefinitions) =
      replacePropertiesSchemaWithReferenceAndCollectDefinitions(p.copy(json = relocatingJson))

    val modifiedJson = preservedJson + (conditionalKeyword -> JsArray(
      variants.map(JsonUtils.deepMerge(relocatingPrunedJson, _))))

    if (p.debug.enabled && p.debug.traceReadingProgress) {
      println(s"C ${p.debug.curIndent("=")}${SchemaReferenceResolver.pathToReference(p.path)}")
    }

    if (p.debug.showCompiledObjectJson) {
      p.debug
        .show(
          s"object + $conditionalKeyword: combined json at " + SchemaReferenceResolver.pathToReference(p.path),
          Json.prettyPrint(modifiedJson))
    }

    (modifiedJson, promotedToDefinitions)
  }

  def mergePartialSchemas(p: Parameters, partials: Seq[JsObject]): (JsObject, Seq[Schema], SchemaReferenceResolver) = {

    val (aggregatedJson, definitions, resolvers) = partials.zipWithIndex
      .map {
        case (partialJson, index) =>
          val partialPath = index.toString :: p.path
          preparePartialSchemaToMerge(p.copy(json = partialJson, path = partialPath))
      }
      .reduce { (l, r) =>
        (JsonUtils.deepMerge(l._1, r._1).as[JsObject], l._2 ++ r._2, l._3 ++ r._3)
      }

    if (p.debug.enabled && p.debug.traceReadingProgress) {
      println(s"M ${p.debug.curIndent("=")}${SchemaReferenceResolver.pathToReference(p.path)}")
    }

    (aggregatedJson, definitions, SchemaReferenceResolver(resolvers))
  }

  def preparePartialSchemaToMerge(p: Parameters): (JsObject, Seq[Schema], Seq[SchemaReferenceResolver]) = {
    val json = SchemaUtils.deepResolveReferences(p.json, p.referenceResolver)

    val (pruned, promotedToDefinitions) = replacePropertiesSchemaWithReferenceAndCollectDefinitions(p.copy(json = json))
    val filtered = SchemaUtils.removeKeysFromSchema(pruned, keywordsToRemoveWhenMergingSchema)
    val dereferenced = SchemaUtils.dereferenceOneLevelOfSchema(filtered, p.path, p.referenceResolver)

    val resolvers: Seq[SchemaReferenceResolver] =
      SchemaUtils.collectSchemaReferenceResolvers(json, p.referenceResolver)

    (dereferenced, promotedToDefinitions, resolvers)
  }

  def replacePropertiesSchemaWithReferenceAndCollectDefinitions(p: Parameters): (JsObject, Seq[Schema]) = {
    val promotedToDefinitions = JsonUtils.visitObjectFields(p.json) {
      case (key, jsObject: JsObject) if key == "properties" || key == "patternProperties" =>
        JsonUtils.visitObjectFields(jsObject) {
          case (name, jsObject: JsObject) if jsObject.keys.intersect(Vocabulary.objectAndArrayVocabulary).nonEmpty =>
            Seq(
              readSchema(
                name,
                name :: key :: p.path,
                jsObject,
                None,
                Seq.empty,
                p.referenceResolver,
                processDefinitions = true,
                p.debug))
        }
    }
    val definitionsReferenceMap: Map[String, String] = promotedToDefinitions.groupBy(_.name).mapValues(_.head.uri)
    val retrofittedJson =
      SchemaUtils.replacePropertiesSchemaWithReferenceUsingMap(p.json, definitionsReferenceMap)

    if (p.debug.enabled && p.debug.traceReadingProgress && definitionsReferenceMap.nonEmpty) {
      val indent = p.debug.curIndent("+")
      println(s"E $indent${definitionsReferenceMap.map { case (k, v) => s"$k@$v" }.mkString(s"\n  $indent")}")
    }

    (retrofittedJson, promotedToDefinitions)
  }
}
