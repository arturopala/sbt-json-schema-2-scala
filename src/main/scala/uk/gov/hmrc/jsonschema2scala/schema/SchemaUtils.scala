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

import java.net.{URI, URLEncoder}

import play.api.libs.json._
import uk.gov.hmrc.jsonschema2scala.utils.JsonUtils.{transformArrayValues, transformObjectFields, visitArrayValues, visitObjectFields}
import uk.gov.hmrc.jsonschema2scala.utils.{JsonUtils, OptionOps}

object SchemaUtils {

  def removeKeysFromSchema(schemaJson: JsObject, keysToRemove: Set[String]): JsObject =
    JsObject(schemaJson.fields.filterNot(f => keysToRemove.contains(f._1)))

  def checkKeyExistsAndNonEmpty(schemaJson: JsObject, key: String): Boolean =
    (schemaJson \ key)
      .asOpt[JsValue]
      .exists {
        case jsObject: JsObject   => jsObject.keys.nonEmpty
        case jsArray: JsArray     => jsArray.value.nonEmpty
        case jsBoolean: JsBoolean => jsBoolean.value
        case _                    => true
      }

  def listSchemaUriToSchema(schema: Schema): Seq[(String, Schema)] =
    Seq((schema.uri, schema)) ++ (schema match {
      case s: ObjectSchema =>
        s.properties.flatMap(listSchemaUriToSchema) ++ s.patternProperties
          .map(_.flatMap(listSchemaUriToSchema))
          .getOrElse(Seq.empty) ++ s.additionalProperties
          .map(listSchemaUriToSchema)
          .getOrElse(Seq.empty) ++ s.unevaluatedProperties
          .map(listSchemaUriToSchema)
          .getOrElse(Seq.empty)

      case s: OneOfAnyOfSchema =>
        s.variants.flatMap(listSchemaUriToSchema)

      case arraySchema: ArraySchema =>
        arraySchema.items
          .map(_.flatMap(listSchemaUriToSchema))
          .getOrElse(Seq.empty)

      case _ => Seq.empty
    })

  /**
    * Pushes references, if any, one level down.
    * Returns a schema with references replaced by schema
    * with in turn its properties converted to references.
    */
  def dereferenceOneLevelOfSchema(
    schemaJson: JsObject,
    path: List[String],
    referenceResolver: SchemaReferenceResolver): JsObject =
    (schemaJson \ "$ref")
      .asOpt[String]
      .flatMap { reference =>
        val uri: URI = referenceResolver.resolveUri(URI.create(reference))
        referenceResolver.lookupJson(uri).flatMap {
          case (referencedSchema: JsObject, resolver) =>
            val dereferenced = dereferenceOneLevelOfSchema(referencedSchema, path, resolver)
            val retrofitted = replacePropertiesSchemaWithReferenceUsingBase(dereferenced, uri.toString)
            val resolved = deepResolveReferences(retrofitted, resolver)
            Some(JsonUtils.deepMerge(schemaJson.-("$ref"), resolved).as[JsObject])

          case _ => None
        }
      }
      .getOrElse(schemaJson)

  /**
    * Rebase all nested references to the current base.
    */
  def deepInlineReferences(schemaJson: JsObject, path: List[String], referenceBaseCandidate: String): JsObject = {
    val referenceBase: String = SchemaReader.attemptReadId(schemaJson) match {
      case Some(_) =>
        SchemaReferenceResolver.pathToReference(path)
      case _ => referenceBaseCandidate
    }

    transformObjectFields(schemaJson) {
      case ("$ref", JsString(reference)) =>
        ("$ref", JsString(if (reference.startsWith("#/")) referenceBase + reference.drop(1) else reference))

      case (Vocabulary.holdsJsonObject(keyword), jsObject: JsObject) =>
        (keyword, transformObjectFields(jsObject) {
          case (name, json2: JsObject) => (name, deepInlineReferences(json2, name :: path, referenceBase))
          case (name, other)           => (name, other)
        })

      case (Vocabulary.holdsJsonArray(keyword), jsArray: JsArray) =>
        (keyword, transformArrayValues(jsArray) {
          case (index, json2: JsObject) => deepInlineReferences(json2, index.toString :: path, referenceBase)
          case (_, other)               => other
        })

      case (name, other) => (name, other)
    }
  }

  /**
    * Resolve all nested references to be absolute URIs
    */
  def deepResolveReferences(schemaJson: JsObject, referenceResolverCandidate: SchemaReferenceResolver): JsObject = {
    val referenceResolver: SchemaReferenceResolver =
      schemaReferenceResolverFor(schemaJson, referenceResolverCandidate)

    transformObjectFields(schemaJson) {
      case ("$ref", JsString(reference)) =>
        val uri = referenceResolver.resolveUri(URI.create(reference))
        ("$ref", JsString(uri.toString))

      case (Vocabulary.holdsJsonObject(keyword), jsObject: JsObject) =>
        (keyword, transformObjectFields(jsObject) {
          case (name, json2: JsObject) => (name, deepResolveReferences(json2, referenceResolver))
        })

      case (Vocabulary.holdsJsonArray(keyword), jsArray: JsArray) =>
        (keyword, transformArrayValues(jsArray) {
          case (_, json2: JsObject) => deepResolveReferences(json2, referenceResolver)
        })
    }
  }

  final def encodeForUri(s: String): String = URLEncoder.encode(s, "utf-8")

  def replacePropertiesSchemaWithReferenceUsingBase(schemaJson: JsObject, baseReference: String): JsObject = {
    val baseUri = if (baseReference.contains('#')) baseReference else baseReference + "#"
    JsonUtils.transformObjectFields(schemaJson) {
      case (key, jsObject: JsObject) if key == "properties" || key == "patternProperties" =>
        (key, JsonUtils.transformObjectFields(jsObject) {
          case (name, _: JsObject) =>
            val uri = s"$baseUri/${encodeForUri(key)}/${encodeForUri(name)}"
            (name, JsObject(Map("$ref" -> JsString(uri))))
        })
    }
  }

  def replacePropertiesSchemaWithReferenceUsingMap(
    schemaJson: JsObject,
    fieldReferenceMap: Map[String, String]): JsObject =
    JsonUtils.transformObjectFields(schemaJson) {
      case (key, jsObject: JsObject) if key == "properties" || key == "patternProperties" =>
        (key, JsonUtils.transformObjectFields(jsObject) {
          case (name, _: JsObject) if fieldReferenceMap.contains(name) =>
            val uri = fieldReferenceMap(name)
            (name, JsObject(Map("$ref" -> JsString(uri))))
        })
    }

  def collectSchemaReferenceResolvers(
    json: JsObject,
    referenceResolverCandidate: SchemaReferenceResolver): Seq[SchemaReferenceResolver] = {
    val referenceResolver: SchemaReferenceResolver = schemaReferenceResolverFor(json, referenceResolverCandidate)

    val resolvers = Seq(referenceResolver) ++ visitObjectFields(json) {
      case (Vocabulary.holdsJsonObject(_), jsObject: JsObject) =>
        visitObjectFields(jsObject) {
          case (_, json2: JsObject) => collectSchemaReferenceResolvers(json2, referenceResolver)
        }.distinct

      case (Vocabulary.holdsJsonArray(_), jsArray: JsArray) =>
        visitArrayValues(jsArray) {
          case (_, json2: JsObject) => collectSchemaReferenceResolvers(json2, referenceResolver)
        }.distinct
    }

    resolvers.distinct
  }

  def schemaReferenceResolverFor(
    json: JsObject,
    referenceResolverCandidate: SchemaReferenceResolver): SchemaReferenceResolver =
    SchemaReader
      .attemptReadId(json)
      .map { uri =>
        val uri2 = if (uri.isAbsolute) uri else referenceResolverCandidate.resolveUri(uri)
        SchemaReferenceResolver(uri2, json, Some(referenceResolverCandidate))
      }
      .getOrElse(referenceResolverCandidate)

  final val isNonPrimitiveSchema: Schema => Boolean = {
    case _: ObjectSchema     => true
    case _: ArraySchema      => true
    case s: OneOfAnyOfSchema => !s.primitive
    case s: AllOfSchema      => !s.primitive
    case _                   => false
  }

  final val isEmptySchema: Schema => Boolean = {
    case objectSchema: ObjectSchema   => objectSchema.isEmpty
    case _: NullSchema                => true
    case not: NotSchema               => !isEmptySchema(not.schema)
    case ref: InternalSchemaReference => isEmptySchema(ref.schema)
    case ref: ExternalSchemaReference => isEmptySchema(ref.schema)
    case _                            => false
  }

  def isEffectiveArraySchema: Schema => Boolean = {
    case _: ArraySchema                              => true
    case i: InternalSchemaReference                  => isEffectiveArraySchema(i.schema)
    case e: ExternalSchemaReference                  => isEffectiveArraySchema(e.schema)
    case o: OneOfAnyOfSchema if o.variants.size == 1 => isEffectiveArraySchema(o.variants.head)
    case a: AllOfSchema                              => isEffectiveArraySchema(a.aggregatedSchema)
    case _                                           => false
  }

  def isPossiblyObjectSchema: Schema => Boolean = {
    case _: ObjectSchema            => true
    case _: OneOfAnyOfSchema        => true
    case _: AllOfSchema             => true
    case a: ArraySchema             => a.items.exists(_.exists(isPossiblyObjectSchema))
    case i: InternalSchemaReference => isPossiblyObjectSchema(i.schema)
    case e: ExternalSchemaReference => isPossiblyObjectSchema(e.schema)
    case _                          => false
  }

  def possibleObjectSchemas: Schema => Seq[Schema] = {
    case o: ObjectSchema            => Seq(o)
    case o: OneOfAnyOfSchema        => Seq(o)
    case a: AllOfSchema             => Seq(a)
    case a: ArraySchema             => a.items.map(_.flatMap(possibleObjectSchemas)).getOrElse(Seq.empty)
    case i: InternalSchemaReference => possibleObjectSchemas(i.schema)
    case e: ExternalSchemaReference => possibleObjectSchemas(e.schema)
    case _                          => Seq.empty
  }

  def referencedSchemaOption: Schema => Option[Schema] = {
    case i: InternalSchemaReference => Some(i.schema)
    case e: ExternalSchemaReference => Some(e.schema)
    case _                          => None
  }

  def copy(schema: Schema, name: String): Schema = {
    val newAttributes = schema.attributes.copy(name = name)
    copyAttributes(schema, newAttributes)
  }

  def copy(schema: Schema, name: String, path: List[String], description: Option[String]): Schema = {
    val newAttributes =
      schema.attributes.copy(name = name, path = path, description = description.orElse(schema.description))
    copyAttributes(schema, newAttributes)
  }

  def copyAttributes(schema: Schema, newAttributes: SchemaAttributes): Schema =
    schema match {
      case s: ObjectSchema =>
        s.copy(attributes = newAttributes)
      case s: ArraySchema =>
        s.copy(attributes = newAttributes)
      case s: OneOfAnyOfSchema =>
        s.copy(attributes = newAttributes)
      case s: AllOfSchema =>
        s.copy(attributes = newAttributes)
      case s: NotSchema =>
        s.copy(attributes = newAttributes)
      case s: IfThenElseSchema =>
        s.copy(attributes = newAttributes)
      case s: StringSchema =>
        s.copy(attributes = newAttributes)
      case s: NumberSchema =>
        s.copy(attributes = newAttributes)
      case s: IntegerSchema =>
        s.copy(attributes = newAttributes)
      case s: BooleanSchema =>
        s.copy(attributes = newAttributes)
      case s: NullSchema =>
        s.copy(attributes = newAttributes)
      case s: InternalSchemaReference =>
        s.copy(attributes = newAttributes)
      case s: ExternalSchemaReference =>
        s.copy(attributes = newAttributes)
      case s: SchemaStub =>
        s.copy(attributes = newAttributes)
    }

}
