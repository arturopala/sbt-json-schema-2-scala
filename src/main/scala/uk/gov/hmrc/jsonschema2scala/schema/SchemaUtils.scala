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

  def copy(schema: Schema, name: String, path: List[String], description: Option[String]): Schema = {
    val newAttributes =
      schema.attributes.copy(name = name, path = path, description = description.orElse(schema.description))
    schema match {
      case s: ObjectSchema =>
        s.copy(attributes = newAttributes)
      case s: ArraySchema =>
        s.copy(attributes = newAttributes)
      case s: MapSchema =>
        s.copy(attributes = newAttributes)
      case s: OneOfAnyOfSchema =>
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
}
