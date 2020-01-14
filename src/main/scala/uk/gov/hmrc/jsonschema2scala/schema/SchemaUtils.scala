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

object SchemaUtils {

  def listSchemaUriToSchema(schema: Schema): Seq[(String, Schema)] =
    Seq((schema.uri, schema)) ++ (schema match {
      case s: ObjectSchema =>
        s.properties.flatMap(listSchemaUriToSchema) ++ s.patternProperties
          .map(_.flatMap(listSchemaUriToSchema))
          .getOrElse(Seq.empty)
      case s: MapSchema =>
        s.patternProperties.flatMap(listSchemaUriToSchema)
      case s: OneOfAnyOfSchema =>
        s.variants.flatMap(listSchemaUriToSchema)
      case arraySchema: ArraySchema => arraySchema.items.map(_.flatMap(listSchemaUriToSchema)).getOrElse(Seq.empty)
      case _                        => Seq.empty
    })

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
      case s: MapSchema =>
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

  /**
    * Pushes references, if any, one level down.
    * Returns a schema with a direct reference replaced by referenced schema
    * with its properties converted to be a referenced schemas.
    */
  def dereferenceSchema(
    json: JsObject,
    path: List[String],
    referenceResolver: SchemaReferenceResolver,
    fieldsToRemove: Set[String]): JsValue = {

    val stage1 = deepResolveReferences(json, referenceResolver)
    val stage2 = JsObject(stage1.fields.filterNot(f => fieldsToRemove.contains(f._1)))

    (stage2 \ "$ref")
      .asOpt[String]
      .flatMap { reference =>
        val uri: URI = referenceResolver.resolveUri(URI.create(reference))
        referenceResolver.lookupJson(uri).map {
          case (referencedSchema: JsObject, resolver) =>
            dereferenceSchema(referencedSchema, path, resolver, fieldsToRemove) match {
              case jsObject: JsObject =>
                val retrofitted = replaceObjectSchemaPropertiesWithReferences(jsObject, uri.toString)
                stage2.-("$ref").deepMerge(retrofitted)

              case other => other
            }

          case (other, _) => other
        }
      }
      .getOrElse(stage2)
  }

  /**
    * Convert each property schema to a reference
    */
  def replaceObjectSchemaPropertiesWithReferences(json: JsObject, baseReference: String): JsObject =
    (json \ "properties").asOpt[JsObject] match {
      case Some(properties) =>
        val newProperties = JsObject(properties.fields.map {
          case (name, value) =>
            (name, value match {
              case _: JsObject =>
                Json.obj("$ref" -> s"$baseReference/properties/$name")

              case other => other
            })
        })
        json.-("properties").+("properties" -> newProperties)

      case None => json
    }

  /**
    * Rebase all nested references to the current base.
    */
  def deepInlineReferences(json: JsObject, path: List[String], referenceBaseCandidate: String): JsObject = {
    val referenceBase: String = SchemaReader.attemptReadId(json) match {
      case Some(_) =>
        SchemaReferenceResolver.pathToReference(path)
      case _ => referenceBaseCandidate
    }

    transformObjectFields(json) {
      case ("$ref", JsString(reference)) =>
        ("$ref", JsString(if (reference.startsWith("#/")) referenceBase + reference.drop(1) else reference))

      case ("properties", jsObject: JsObject) =>
        ("properties", transformObjectFields(jsObject) {
          case (name, json2: JsObject) => (name, deepInlineReferences(json2, name :: path, referenceBase))
          case (name, other)           => (name, other)
        })

      case (name, other) => (name, other)
    }
  }

  /**
    * Resolve all nested references to be absolute URIs
    */
  def deepResolveReferences(json: JsObject, referenceResolverCandidate: SchemaReferenceResolver): JsObject = {
    val referenceResolver: SchemaReferenceResolver = schemaReferenceResolverFor(json, "", referenceResolverCandidate)

    transformObjectFields(json) {
      case ("$ref", JsString(reference)) =>
        val uri = referenceResolver.resolveUri(URI.create(reference))
        ("$ref", JsString(uri.toString))

      case ("properties", jsObject: JsObject) =>
        ("properties", transformObjectFields(jsObject) {
          case (name, json2: JsObject) => (name, deepResolveReferences(json2, referenceResolver))
        })
    }
  }

  def collectSchemaReferenceResolvers(
    json: JsObject,
    referenceResolverCandidate: SchemaReferenceResolver): Seq[SchemaReferenceResolver] = {
    val referenceResolver: SchemaReferenceResolver = schemaReferenceResolverFor(json, "", referenceResolverCandidate)

    val resolvers = Seq(referenceResolver) ++ visitObjectFields(json) {
      case ("properties", jsObject: JsObject) =>
        visitObjectFields(jsObject) {
          case (_, json2: JsObject) => collectSchemaReferenceResolvers(json2, referenceResolver)
        }.distinct
    }

    resolvers.distinct
  }

  def schemaReferenceResolverFor(
    json: JsObject,
    schemaName: String,
    referenceResolverCandidate: SchemaReferenceResolver): SchemaReferenceResolver =
    SchemaReader
      .attemptReadId(json)
      .map { uri =>
        val uri2 = if (uri.isAbsolute) uri else referenceResolverCandidate.resolveUri(uri)
        SchemaReferenceResolver(uri2, schemaName, json, Some(referenceResolverCandidate))
      }
      .getOrElse(referenceResolverCandidate)

  def transformObjectFields(json: JsObject)(fx: PartialFunction[(String, JsValue), (String, JsValue)]): JsObject =
    JsObject(json.fields.map {
      case (name, value) => if (fx.isDefinedAt((name, value))) fx((name, value)) else (name, value)
    })

  def visitObjectFields[T](json: JsObject)(fx: PartialFunction[(String, JsValue), Seq[T]]): Seq[T] =
    json.fields.flatMap { case (name, value) => if (fx.isDefinedAt((name, value))) fx((name, value)) else Seq.empty }

  def transformArray(json: JsArray)(fx: PartialFunction[JsValue, JsValue]): JsArray =
    JsArray(json.value.map(value => if (fx.isDefinedAt(value)) fx(value) else value))

  def visitArray[T](json: JsArray)(fx: PartialFunction[JsValue, Seq[T]]): Seq[T] =
    json.value.flatMap(value => if (fx.isDefinedAt(value)) fx(value) else Seq.empty)
}
