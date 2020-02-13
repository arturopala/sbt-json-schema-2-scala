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
import uk.gov.hmrc.jsonschema2scala.schema.Keywords.{`$defs`, allOf, anyOf, oneOf}
import uk.gov.hmrc.jsonschema2scala.utils.OptionOps.{defined, emptyAsNone}
import uk.gov.hmrc.jsonschema2scala.utils.SeqOps.{hasSingle, isEmpty}
import uk.gov.hmrc.jsonschema2scala.utils.{JsonUtils, NameUtils}

import scala.util.Try

object SchemaReader {

  def read(schemaSource: SchemaSource, resolver: SchemaReferenceResolver, debug: DebugOptions): Schema = {
    val path = SchemaReferenceResolver.rootPath(schemaSource.uri)
    resolver.lookupSchema(
      schemaSource.uri,
      Some(
        (_, jsObject, _, enableDebug) =>
          readSchema(Parameters(
            name = schemaSource.name,
            path = path,
            json = jsObject,
            description = None,
            requiredFields = Seq.empty,
            required = false,
            referenceResolver = resolver,
            processDefinitions = true,
            definitions = Seq.empty,
            debug = debug.copy(enabled = enableDebug.getOrElse(debug.enabled))
          )))
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
    custom: Option[Map[String, JsValue]] = None,
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

  def readSchema(p0: Parameters): Schema = {

    val required = p0.requiredFields.contains(p0.name)

    val id: Option[URI] = attemptReadId(p0.json)

    val (referenceResolver, path) =
      updatedResolverAndPath(p0.name, p0.path, p0.json, p0.referenceResolver, id)

    if (p0.debug.enabled && p0.debug.traceReadingProgress) {
      println(s"> ${p0.debug.incIndent(".")} ${SchemaReferenceResolver.pathToReference(path)}")
    }

    val description: Option[String] = p0.description.orElse(attemptReadDescription(p0.json))

    val definitions: Seq[Schema] =
      if (p0.processDefinitions)
        readDefinitions("definitions", p0.name, path, p0.json, description, referenceResolver, p0.debug) ++
          readDefinitions("$defs", p0.name, path, p0.json, description, referenceResolver, p0.debug)
      else Seq.empty

    val custom: Option[Map[String, JsValue]] = {
      val set = keywordsNotInVocabulary(p0.json.fields)
      if (set.isEmpty) None else Some(set.map(k => (k, (p0.json \ k).as[JsValue])).toMap)
    }

    val p = p0.copy(
      path = path,
      description = description,
      definitions = definitions,
      required = required,
      custom = custom,
      referenceResolver = referenceResolver)

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
          attributes = p.a
        )
      }

    if (p.debug.enabled && p.debug.traceReadingProgress) {
      println(s"< ${p.debug.decIndent("-")} ${schema.toString}")
    }

    schema
  }

  def readSchemaIfBoolean(name: String, currentPath: List[String], boolean: JsBoolean): Option[Schema] =
    boolean.value match {
      case true  => Some(ObjectSchema(SchemaAttributes(name, currentPath, None, Seq.empty, false, None)))
      case false => None
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
        case JsString(schemaType) =>
          readSchemaWithType(p, schemaType)

        case JsArray(schemaTypeArray) =>
          readSchemaWithArrayOfTypes(p, schemaTypeArray)

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

  def readSchemaWithArrayOfTypes(p: Parameters, schemaTypeArray: Seq[JsValue]): Schema = {

    if (p.debug.enabled && p.debug.traceReadingProgress) {
      println(s"A ${p.debug.curIndent("a")} processing an array of types:[${schemaTypeArray.mkString(", ")}]")
    }

    val p2 = p.copy(definitions = Seq.empty)

    val variants: Seq[Schema] = schemaTypeArray.distinct.map {
      case JsString(valueType) =>
        valueType match {
          case "string"  => readStringSchema(p2)
          case "number"  => readNumberSchema(p2)
          case "integer" => readIntegerSchema(p2)
          case "boolean" => readBooleanSchema(p2)
          case "object"  => readObjectSchema(p2)
          case "array"   => readArraySchema(p2)
          case "null"    => NullSchema(p2.a)
          case other =>
            throw new IllegalStateException(
              s"Invalid type name, expected one of [null, boolean, object, array, number, integer, string], but got $other")
        }
      case other =>
        throw new IllegalStateException(s"Invalid type definition, expected an array of strings, but got $other")
    }

    wrapVariantsAsSchema(p, variants)
      .getOrElse(
        throw new IllegalStateException(s"Invalid schema, expected non empty set of variants but got empty.")
      )
  }

  def wrapVariantsAsSchema(p: Parameters, variants: Seq[Schema]): Option[Schema] = {

    val filteredVariants = variants /*.filterNot(isEmptySchema)*/ .distinct
    val (explodedVariants, definitions) = explodeOneOfAnyOfVariants(filteredVariants)

    explodedVariants match {
      case isEmpty()       => None
      case hasSingle(item) => Some(item.addDefinitions(p.definitions ++ definitions))
      case manyItems       => Some(OneOfAnyOfSchema(p.a, manyItems, isOneOf = true).addDefinitions(definitions))

    }
  }

  def attemptReadReference(p: Parameters): Option[Schema] =
    (p.json \ "$ref")
      .asOpt[String]
      .map(readReference(p, _))

  def readReference(p: Parameters, reference: String): Schema = {

    val uri: URI = p.referenceResolver.resolveUri(URI.create(reference))
    val path2 = p.referenceResolver.uriToPath(uri)

    if (p.debug.enabled && p.debug.traceReadingProgress) {
      println(s"R ${p.debug.curIndent(">")} $uri")
    }

    val schema: Schema = p.referenceResolver
      .lookupSchema(
        uri,
        Some(
          (name, json, resolver, enableDebug) =>
            readSchema(
              p.copy(
                name = name,
                path = path2,
                json = json,
                referenceResolver = resolver,
                processDefinitions = true,
                debug = p.debug.copy(enabled = enableDebug.getOrElse(p.debug.enabled)))
          ))
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
                .flatMap(uri =>
                  resolver.lookupSchema(
                    uri,
                    Some((name, value, resolver, enableDebug) =>
                      readSchema(p.copy(
                        name = name,
                        path = Nil,
                        json = value,
                        description = None,
                        requiredFields = Seq.empty,
                        referenceResolver = resolver,
                        processDefinitions = true,
                        debug = p.debug.copy(enabled = enableDebug.getOrElse(p.debug.enabled))
                      )))
                ))
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

    schema
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
      //.filterNot(isEmptySchema)
      .orElse {
        attemptReadEnumOrConst(p)
      }
  }

  def readObjectSchema(p: Parameters): Schema = {

    val (required, alternatives) = readRequired(p.json)
    val p2 = p.copy(requiredFields = required)

    val hasProperties = SchemaUtils.checkKeyExistsAndNonEmpty(p2.json, "properties")
    val hasPatternProperties = SchemaUtils.checkKeyExistsAndNonEmpty(p2.json, "patternProperties")
    val hasAdditionalProperties = SchemaUtils.checkKeyExistsAndNonEmpty(p2.json, "additionalProperties")
    val hasUnevaluatedProperties = SchemaUtils.checkKeyExistsAndNonEmpty(p2.json, "unevaluatedProperties")

    val minPropertiesOpt = (p2.json \ "minProperties").asOpt[Int]
    val maxPropertiesOpt = (p2.json \ "maxProperties").asOpt[Int]

    val isDefined = hasProperties || hasPatternProperties || hasAdditionalProperties ||
      hasUnevaluatedProperties || minPropertiesOpt.isDefined || maxPropertiesOpt.isDefined

    val extras: Map[String, Seq[JsObject]] =
      readSchemaFields(p2, Set(oneOf, anyOf, allOf), Vocabulary.schemaKeyVocabulary)

    val schema = if (extras.nonEmpty) {
      if (!isDefined && extras.size == 1) {
        extras.head match {

          case (name, hasSingle(json)) =>
            val s = readSchema(p2.copy(json = json, path = "0" :: name :: p.path))
            s.addDefinitions(p2.definitions)

          case _ =>
            if (p2.debug.enabled && p2.debug.traceReadingProgress) {
              println(s"O ${p2.debug.curIndent("o")} reading empty object with extras:${extras
                .map { case (k, vs) => s"$k:${vs.size}" }
                .mkString("{", ", ", "}")}")
            }
            readObjectSchemaWithExtras(p2, extras)
        }
      } else {
        if (p2.debug.enabled && p2.debug.traceReadingProgress) {
          println(
            s"O ${p2.debug.curIndent("o")} reading object with extras:${extras.map { case (k, vs) => s"$k:${vs.size}" }.mkString("{", ", ", "}")}")
        }
        readObjectSchemaWithExtras(p2, extras)
      }
    } else {

      val propertiesOpt: Option[Seq[Schema]] =
        attemptReadProperties(p2, "properties").flatMap(emptyAsNone)

      val patternPropertiesOpt: Option[Seq[Schema]] =
        attemptReadProperties(p2, "patternProperties").flatMap(emptyAsNone)

      val additionalPropertiesOpt: Option[Schema] =
        attemptReadAdditionalProperties(p2, "additionalProperties", "Property")

      val unevaluatedPropertiesOpt: Option[Schema] =
        attemptReadAdditionalProperties(
          p2,
          "unevaluatedProperties",
          if (additionalPropertiesOpt.isEmpty) "Property" else "Property_2")

      ObjectSchema(
        attributes = p2.a,
        properties = propertiesOpt.getOrElse(Seq.empty),
        requiredFields = required,
        alternativeRequiredFields = alternatives,
        patternProperties = patternPropertiesOpt,
        additionalProperties = additionalPropertiesOpt,
        unevaluatedProperties = unevaluatedPropertiesOpt,
        minProperties = minPropertiesOpt,
        maxProperties = maxPropertiesOpt
      )
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

  def attemptReadProperties(p: Parameters, key: String): Option[Seq[Schema]] =
    (p.json \ key)
      .asOpt[JsObject]
      .map(properties =>
        properties.fields
          .map(_._1)
          .distinct
          .map(name => {
            (properties \ name).as[JsValue] match {
              case jsObject: JsObject =>
                Some(readSchema(p.copy(name = name, path = name :: key :: p.path, json = jsObject)))

              case jsBoolean: JsBoolean =>
                readSchemaIfBoolean(name = name, currentPath = name :: key :: p.path, boolean = jsBoolean)

              case other =>
                throw new IllegalStateException(
                  s"Invalid object schema, expected property $key/$name to be an object or boolean, but got ${other.getClass.getSimpleName} at ${p.currentUri}")
            }
          })
          .collect(defined))

  def attemptReadAdditionalProperties(p: Parameters, key: String, suffix: String): Option[Schema] =
    (p.json \ key)
      .asOpt[JsValue]
      .flatMap {
        case json: JsObject =>
          val schema = readSchema(p.copy(json = json, path = key :: p.path, description = None)) match {
            case s if s.name != p.name => s
            case s                     => s.withName(s"${s.name}$suffix")
          }
          Some(schema)

        case jsArray: JsArray =>
          val variants = jsArray.value.zipWithIndex.collect {
            case (json: JsObject, index) =>
              readSchema(p.copy(json = json, path = index.toString :: key :: p.path, description = None))
          }
          wrapVariantsAsSchema(p, variants)

        case jsBoolean: JsBoolean if jsBoolean.value =>
          Some(ObjectSchema(p.a))

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
                p.copy(
                  name = NameUtils.singular(p.name),
                  path = "items" :: p.path,
                  json = itemSchema,
                  requiredFields = Seq(p.name))
              )))

        case array: JsArray =>
          array.value.toList match {
            case Nil => None
            case many =>
              Some(many.zipWithIndex map {
                case (json: JsObject, index) =>
                  readSchema(
                    p.copy(
                      name = NameUtils.singular(p.name),
                      path = index.toString :: "items" :: p.path,
                      json = json,
                      requiredFields = Seq(p.name)))

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
          println(s"D ${debug
            .curIndent("#")} about to read definitions:${properties.fields.map(_._1).distinct.mkString("[", ",", "]")}")
        }
        val definitions: Seq[Schema] = properties.fields
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
                Some((name, value, resolver, enableDebug) =>
                  readSchema(Parameters(
                    name = name,
                    path = path2,
                    json = value,
                    description = description,
                    requiredFields = Seq.empty,
                    required = false,
                    referenceResolver = resolver,
                    processDefinitions = true,
                    definitions = Seq.empty,
                    debug = debug.copy(enabled = enableDebug.getOrElse(debug.enabled))
                  )))
              )
              .map(_._1)
          })
          .collect { case Some(x) => x }

        if (debug.enabled && debug.traceReadingProgress) {
          println(
            s"D ${debug.curIndent("#")} finished reading definitions:${properties.fields.map(_._1).distinct.mkString("[", ",", "]")}")
        }

        definitions
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
      .flatMap { enum =>
        val types = enum.groupBy(_.getClass.getSimpleName).values.toSeq

        types.size match {
          case 1 =>
            Some(enum.head match {
              case JsNull       => NullSchema(p.a)
              case _: JsString  => StringSchema(p.a, enum = Some(enum.map(_.as[String])))
              case _: JsNumber  => NumberSchema(p.a, enum = Some(enum.map(_.as[BigDecimal])))
              case _: JsBoolean => BooleanSchema(p.a, enum = Some(enum.map(_.as[Boolean])))
              case _: JsArray   => ArraySchema(p.a)
              case other =>
                throw new IllegalStateException(s"Unsupported feature, enum of [$other] at ${p.currentUri}")
            })
          case _ =>
            val variants: Seq[Schema] = types.map {
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

            wrapVariantsAsSchema(p, variants)
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

    def readVariant(path: List[String], jsValue: JsValue): Seq[Schema] = jsValue match {
      case jsObject: JsObject =>
        Seq(readSchema(p.copy(path = path, json = jsObject)))

      case jsArray: JsArray =>
        jsArray.value.zipWithIndex.flatMap {
          case (value, index) => readVariant(index.toString :: path, value)
        }

      case other =>
        throw new IllegalStateException(s"Invalid ${if (isOneOf) oneOf else anyOf} schema, expected ${p.path.reverse
          .mkString("/")} to be an object or array, but got ${other.getClass.getSimpleName}.")
    }

    val variants: Seq[Schema] = readVariant((if (isOneOf) oneOf else anyOf) :: p.path, array)

    wrapVariantsAsSchema(p, variants)
  }

  def explodeOneOfAnyOfVariants(variants: Seq[Schema]): (Seq[Schema], Seq[Schema]) =
    variants.foldLeft((Seq.empty[Schema], Seq.empty[Schema])) {
      case ((vs, ds), v) =>
        v match {
          case oneOfAnyOfSchema: OneOfAnyOfSchema =>
            val (vs1, ds1) = explodeOneOfAnyOfVariants(oneOfAnyOfSchema.variants)
            (vs ++ vs1, ds ++ oneOfAnyOfSchema.definitions ++ ds1)
          case other => (vs :+ other, ds)
        }
    }

  def explodeAllOfPartials(partials: Seq[Schema]): (Seq[Schema], Seq[Schema]) =
    partials.foldLeft((Seq.empty[Schema], Seq.empty[Schema])) {
      case ((vs, ds), v) =>
        v match {
          case allOfSchema: AllOfSchema =>
            val (vs1, ds1) = explodeOneOfAnyOfVariants(allOfSchema.partials)
            (vs ++ vs1, ds ++ allOfSchema.definitions ++ ds1)
          case other => (Seq(other), Seq.empty)
        }
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

        case hasSingle(jsValue) =>
          jsValue match {
            case jsObject: JsObject =>
              val schema = readSchema(p.copy(path = "0" :: allOf :: p.path, json = jsObject))
              AllOfSchema(p.a, Seq.empty, p.requiredFields, schema)

            case other =>
              throw new IllegalStateException(s"Invalid allOf schema, expected ${p.path.reverse
                .mkString("/")}/0 to be an object, but got ${other.getClass.getSimpleName}.")
          }

        case arrayValues => {
          val (partials, explodedDefinitions) = {
            val ps = arrayValues.zipWithIndex.map {
              case (jsObject: JsObject, i) =>
                readSchema(p.copy(path = i.toString :: allOf :: p.path, json = jsObject))

              case (other, i) =>
                throw new IllegalStateException(s"Invalid allOf schema, expected ${p.path.reverse
                  .mkString("/")}/$i to be an object, but got ${other.getClass.getSimpleName}.")
            }
            explodeAllOfPartials(ps)
          }

          val (mergedJson, promotedToDefinitions, resolver) =
            mergePartialSchemas(p.copy(path = allOf :: p.path), arrayValues.collect {
              case jsObject: JsObject => jsObject
            })

          val aggregatedSchema: Schema =
            readSchema(p.copy(json = mergedJson, referenceResolver = resolver, processDefinitions = false))

          AllOfSchema(p.a, partials, p.requiredFields, aggregatedSchema)
            .addDefinitions(promotedToDefinitions ++ explodedDefinitions)
        }
      }

  def attemptReadNot(p: Parameters): Option[Schema] =
    (p.json \ "not")
      .asOpt[JsObject]
      .map { json =>
        val schema: Schema = readSchema(p.copy(path = "not" :: p.path, json = json))
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
              readSchema(p.copy(path = "if" :: p.path, json = ifJson))

            val schema: Schema =
              readSchema(p.copy(path = "then" :: p.path, json = thenJson))

            val elseSchema: Option[Schema] = (p.json \ "else")
              .asOpt[JsObject]
              .map { elseJson =>
                readSchema(p.copy(path = "else" :: p.path, json = elseJson))
              }

            IfThenElseSchema(p.a, condition, schema, elseSchema)
          }
      }

  def updatedResolverAndPath(
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
      SchemaUtils
        .deepResolveReferences(p.json - allOf - oneOf - anyOf - Keywords.definitions - `$defs`, p.referenceResolver)

    val (stage1Json, stage1Definitions, stage1Resolver) = extras
      .get(allOf)
      .map { partials =>
        val (aggregatedJson, definitions, resolver) = mergePartialSchemas(p.copy(path = allOf :: p.path), partials)
        val mergedJson = JsonUtils.deepMerge(coreObjectJson, aggregatedJson).as[JsObject]
        (mergedJson, definitions, resolver)
      }
      .getOrElse((coreObjectJson, Seq.empty, p.referenceResolver))

    val (stage2Json, stage2Definitions) = (extras.get(oneOf), extras.get(anyOf)) match {
      case (None, None) => (stage1Json, Seq.empty)
      case (Some(variants), None) =>
        embedPropertiesIntoVariants(p.copy(json = stage1Json), oneOf, variants)
      case (None, Some(variants)) =>
        embedPropertiesIntoVariants(p.copy(json = stage1Json), anyOf, variants)
      case (Some(variants1), Some(variants2)) =>
        throw new IllegalStateException(
          s"Unsupported schema with both oneOf and anyOf at ${SchemaReferenceResolver.pathToReference(p.path)}")
    }

    if (p.debug.showJsonAfterInliningConditionals) {
      p.debug
        .show(
          s"object + extras [${extras.keys.mkString(",")}]: combined json at " + SchemaReferenceResolver
            .pathToReference(p.path),
          Json.prettyPrint(stage2Json))
    }

    val schema: Schema = readSchema(p.copy(json = stage2Json, referenceResolver = stage1Resolver))

    schema.addDefinitions(p.definitions ++ stage1Definitions ++ stage2Definitions)
  }

  def embedPropertiesIntoVariants(
    p: Parameters,
    conditionalKeyword: String,
    variants: Seq[JsObject]): (JsObject, Seq[Schema]) = {

    val preservedVocabulary = Vocabulary.metaCoreVocabulary - Keywords.definitions - `$defs`
    val preservedJson = JsonUtils.filterObjectFields(p.json)(preservedVocabulary.contains)

    if (preservedJson.keys.isEmpty) {
      (preservedJson + (conditionalKeyword -> JsArray(variants)), Seq.empty)
    } else {

      if (p.debug.enabled && p.debug.traceReadingProgress) {
        println(
          s"C ${p.debug.curIndent("=")} combining object with ${variants.size} variants $conditionalKeyword@${SchemaReferenceResolver
            .pathToReference(p.path)}")
      }

      val relocatingJson = JsonUtils.filterObjectFields(p.json)(Vocabulary.objectVocabulary.contains)

      val (relocatingPrunedJson, promotedToDefinitions) =
        replacePropertiesSchemaWithReferenceAndCollectDefinitions(p.copy(json = relocatingJson))

      val modifiedJson = preservedJson + (conditionalKeyword -> JsArray(variants.zipWithIndex.map {
        case (variant, index) =>
          val dereferencedVariant =
            SchemaUtils.dereferenceOneLevelOfSchema(variant, index.toString :: p.path, p.referenceResolver)
          JsonUtils.deepMerge(relocatingPrunedJson, dereferencedVariant)
      }))

      if (p.debug.showJsonAfterEmbeddingPropertiesIntoVariants) {
        p.debug
          .show(
            s"object + $conditionalKeyword: combined json at " + SchemaReferenceResolver.pathToReference(p.path),
            Json.prettyPrint(modifiedJson))
      }

      (modifiedJson, promotedToDefinitions)
    }
  }

  def mergePartialSchemas(p: Parameters, partials: Seq[JsObject]): (JsObject, Seq[Schema], SchemaReferenceResolver) = {

    if (p.debug.enabled && p.debug.traceReadingProgress) {
      println(
        s"M ${p.debug.curIndent("=")} merging ${partials.size} partials allOf@${SchemaReferenceResolver.pathToReference(
          p.path)}")
    }

    val (aggregatedJson, definitions, resolvers) = partials.zipWithIndex
      .map {
        case (partialJson, index) =>
          val partialPath = index.toString :: p.path
          preparePartialSchemaToMerge(p.copy(json = partialJson, path = partialPath))
      }
      .reduce { (l, r) =>
        (JsonUtils.deepMerge(l._1, r._1).as[JsObject], l._2 ++ r._2, l._3 ++ r._3)
      }

    if (p.debug.showJsonAfterAggregatingPartials) {
      p.debug
        .show(
          s"object + allOf: aggregated json at " + SchemaReferenceResolver.pathToReference(p.path),
          Json.prettyPrint(aggregatedJson))
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
                p.copy(
                  name = name,
                  path = name :: key :: p.path,
                  json = jsObject,
                  description = None,
                  requiredFields = Seq.empty,
                  processDefinitions = true)))
        }
    }
    val definitionsReferenceMap: Map[String, String] = promotedToDefinitions.groupBy(_.name).mapValues(_.head.uri)
    val retrofittedJson =
      SchemaUtils.replacePropertiesSchemaWithReferenceUsingMap(p.json, definitionsReferenceMap)

    if (p.debug.enabled && p.debug.traceReadingProgress && definitionsReferenceMap.nonEmpty) {
      val indent = p.debug.curIndent("+")
      println(
        s"E $indent extracting as definition:\n  $indent ${definitionsReferenceMap.map { case (k, v) => s"$k@$v" }.mkString(s"\n  $indent ")}")
    }

    (retrofittedJson, promotedToDefinitions)
  }
}
