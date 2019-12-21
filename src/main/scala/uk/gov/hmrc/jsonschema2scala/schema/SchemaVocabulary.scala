package uk.gov.hmrc.jsonschema2scala.schema

import play.api.libs.json.JsValue

object SchemaVocabulary {

  def keywordsIn(v: Set[String])(fields: Seq[(String, JsValue)]): List[String] =
    fields.foldLeft(List.empty[String])((l, f) => if (v.contains(f._1)) f._1 :: l else l)

  def isKeywordIn(v: Set[String])(fields: Seq[String]): Boolean =
    fields.foldLeft(false)((b, f) => b || v.contains(f))

  final val metaCoreVocabulary: Set[String] = Set(
    "$schema",
    "$vocabulary",
    "$id",
    "$def",
    "$ref",
    "$comment",
    "$anchor",
    "definitions",
    "title",
    "description",
    "default",
    "deprecated",
    "readOnly",
    "writeOnly",
    "examples"
  )

  final val objectCoreVocabulary: Set[String] =
    Set("properties", "patternProperties", "additionalProperties", "unevaluatedProperties", "propertyNames")

  final val arrayCoreVocabulary: Set[String] = Set("items", "additionalItems", "unevaluatedItems", "contains")

  final val conditionalCoreVocabulary: Set[String] = Set(
    "allOf",
    "anyOf",
    "oneOf",
    "not",
    "if",
    "then",
    "else",
    "dependentSchemas"
  )

  final val coreVocabulary: Set[String] = metaCoreVocabulary ++
    objectCoreVocabulary ++
    arrayCoreVocabulary ++
    conditionalCoreVocabulary

  final val objectValidationVocabulary: Set[String] = Set(
    "maxProperties",
    "minProperties",
    "required",
    "dependentRequired"
  )

  final val arrayValidationVocabulary: Set[String] = Set(
    "maxItems",
    "minItems",
    "uniqueItems",
    "maxContains",
    "minContains"
  )

  final val stringValidationVocabulary: Set[String] = Set(
    "maxLength",
    "minLength",
    "pattern"
  )

  final val numberValidationVocabulary: Set[String] = Set(
    "multipleOf",
    "maximum",
    "exclusiveMaximum",
    "minimum",
    "exclusiveMinimum"
  )

  final val validationVocabulary: Set[String] = Set(
    "type",
    "enum",
    "const",
    "format",
    "contentEncoding",
    "contentMediaType",
    "contentSchema"
  ) ++
    objectValidationVocabulary ++
    arrayValidationVocabulary ++
    stringValidationVocabulary ++
    numberValidationVocabulary

  final val vocabulary: Set[String] = coreVocabulary ++ validationVocabulary

  final val vocabularyWithoutMeta: Set[String] = vocabulary.diff(metaCoreVocabulary)

}
