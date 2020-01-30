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

import play.api.libs.json.JsValue

import scala.collection.GenSet

object Vocabulary {
  import Keywords._

  def keywordsIn(v: Set[String])(fields: Seq[(String, JsValue)]): Set[String] =
    fields.map(_._1).toSet.intersect(v)

  def keywordsNotIn(v: Set[String])(fields: Seq[(String, JsValue)]): Set[String] =
    fields.map(_._1).toSet.diff(v)

  def isKeywordIn(v: Set[String])(fields: Seq[String]): Boolean =
    fields.foldLeft(false)((b, f) => b || v.contains(f))

  final val metaCoreVocabulary: Set[String] = Set(
    `$schema`,
    `$vocabulary`,
    `$id`,
    `$defs`,
    `$ref`,
    `$recursiveRef`,
    `$comment`,
    `$anchor`,
    `$recursiveAnchor`,
    definitions,
    title,
    description,
    default,
    deprecated,
    readOnly,
    writeOnly,
    examples
  )

  final val objectCoreVocabulary: Set[String] =
    Set(properties, patternProperties, additionalProperties, unevaluatedProperties)

  final val arrayCoreVocabulary: Set[String] = Set(items, additionalItems, unevaluatedItems)

  final val conditionalCoreVocabulary: Set[String] = Set(
    allOf,
    anyOf,
    oneOf,
    not,
    `if`,
    `then`,
    `else`,
    dependentSchemas
  )

  final val coreVocabulary: Set[String] = metaCoreVocabulary ++
    objectCoreVocabulary ++
    arrayCoreVocabulary ++
    conditionalCoreVocabulary

  final val objectValidationVocabulary: Set[String] = Set(
    maxProperties,
    minProperties,
    required,
    dependentRequired,
    propertyNames
  )

  final val arrayValidationVocabulary: Set[String] = Set(
    maxItems,
    minItems,
    uniqueItems,
    maxContains,
    minContains,
    contains
  )

  final val stringValidationVocabulary: Set[String] = Set(
    maxLength,
    minLength,
    pattern
  )

  final val numberValidationVocabulary: Set[String] = Set(
    multipleOf,
    maximum,
    exclusiveMaximum,
    minimum,
    exclusiveMinimum
  )

  final val commonValidationVocabulary: Set[String] = Set(
    `type`,
    enum,
    const,
    format,
    contentEncoding,
    contentMediaType,
    contentSchema
  )

  final val validationVocabulary: Set[String] = commonValidationVocabulary ++
    objectValidationVocabulary ++
    arrayValidationVocabulary ++
    stringValidationVocabulary ++
    numberValidationVocabulary

  final val objectVocabulary = objectCoreVocabulary ++ objectValidationVocabulary
  final val arrayVocabulary = arrayCoreVocabulary ++ arrayValidationVocabulary
  final val objectAndArrayVocabulary: Set[String] = objectVocabulary ++ arrayVocabulary
  final val oneAnyAllKeywords = Seq(oneOf, anyOf, allOf)
  final val schemaKeyVocabulary = conditionalCoreVocabulary ++ objectCoreVocabulary ++ arrayCoreVocabulary
  final val allKeywords: Set[String] = coreVocabulary ++ validationVocabulary
  final val allKeywordsButMeta: Set[String] = allKeywords.diff(metaCoreVocabulary)
  final val allKeywordsButObject: Set[String] = allKeywords.diff(objectVocabulary)
  final val jsonObjectKeywords: Set[String] =
    Set(properties, additionalProperties, patternProperties, definitions, items)
  final val jsonArrayKeywords: Set[String] = Set(items, additionalItems, allOf, anyOf, oneOf, not)

  object holdsJsonObject {
    def unapply(keyword: String): Option[String] =
      Some(keyword).filter(jsonObjectKeywords.contains)
  }

  object holdsJsonArray {
    def unapply(keyword: String): Option[String] =
      Some(keyword).filter(jsonArrayKeywords.contains)
  }

  case class isInVocabulary(vocabulary: Set[String]) {
    def unapply(keyword: String): Option[String] =
      Some(keyword).filter(vocabulary.contains)
  }

  def vocabularyOfSchemaType(schema: Schema): Set[String] = schema match {
    case _: ObjectSchema                   => objectVocabulary
    case _: ArraySchema                    => arrayVocabulary
    case _: MapSchema                      => Set(patternProperties, required)
    case o: OneOfAnyOfSchema if o.isOneOf  => Set(oneOf)
    case o: OneOfAnyOfSchema if !o.isOneOf => Set(anyOf)
    case _: AllOfSchema                    => Set(allOf)
    case _: NotSchema                      => Set(not)
    case _: IfThenElseSchema               => Set(`if`, `then`, `else`)
    case _: StringSchema                   => stringValidationVocabulary ++ commonValidationVocabulary
    case _: NumberSchema                   => numberValidationVocabulary ++ commonValidationVocabulary
    case _: IntegerSchema                  => numberValidationVocabulary ++ commonValidationVocabulary
    case _: BooleanSchema                  => commonValidationVocabulary
  }

  def conditionalKeyword(schema: Schema): Option[String] = schema match {
    case o: OneOfAnyOfSchema if o.isOneOf  => Some(Keywords.oneOf)
    case o: OneOfAnyOfSchema if !o.isOneOf => Some(Keywords.anyOf)
    case _: AllOfSchema                    => Some(Keywords.allOf)
    case _                                 => None
  }

}
