package uk.gov.hmrc.jsonschema2scala.schema

object Keywords {

  final val `$schema` = "$schema"
  final val `$vocabulary` = "$vocabulary"
  final val `$id` = "$id"
  final val `$defs` = "$defs"
  final val `$ref` = "$ref"
  final val `$recursiveRef` = "$recursiveRef"
  final val `$comment` = "$comment"
  final val `$anchor` = "$anchor"
  final val `$recursiveAnchor` = "$recursiveAnchor"

  final val definitions = "definitions"
  final val title = "title"
  final val description = "description"
  final val default = "default"
  final val deprecated = "deprecated"
  final val readOnly = "readOnly"
  final val writeOnly = "writeOnly"
  final val examples = "examples"

  final val properties = "properties"
  final val patternProperties = "patternProperties"
  final val additionalProperties = "additionalProperties"
  final val unevaluatedProperties = "unevaluatedProperties"
  final val propertyNames = "propertyNames"

  final val items = "items"
  final val additionalItems = "additionalItems"
  final val unevaluatedItems = "unevaluatedItems"
  final val contains = "contains"

  final val allOf = "allOf"
  final val anyOf = "anyOf"
  final val oneOf = "oneOf"
  final val not = "not"
  final val `if` = "if"
  final val `then` = "then"
  final val `else` = "else"
  final val dependentSchemas = "dependentSchemas"

  final val maxProperties = "maxProperties"
  final val minProperties = "minProperties"
  final val required = "required"
  final val dependentRequired = "dependentRequired"

  final val maxItems = "maxItems"
  final val minItems = "minItems"
  final val uniqueItems = "uniqueItems"
  final val maxContains = "maxContains"
  final val minContains = "minContains"

  final val maxLength = "maxLength"
  final val minLength = "minLength"
  final val pattern = "pattern"

  final val multipleOf = "multipleOf"
  final val maximum = "maximum"
  final val exclusiveMaximum = "exclusiveMaximum"
  final val minimum = "minimum"
  final val exclusiveMinimum = "exclusiveMinimum"

  final val `type` = "type"
  final val enum = "enum"
  final val const = "const"
  final val format = "format"
  final val contentEncoding = "contentEncoding"
  final val contentMediaType = "contentMediaType"
  final val contentSchema = "contentSchema"

  final val string = "string"
  final val number = "number"
  final val integer = "integer"
  final val boolean = "boolean"
  final val `object` = "object"
  final val array = "array"
  final val `null` = "null"

}
