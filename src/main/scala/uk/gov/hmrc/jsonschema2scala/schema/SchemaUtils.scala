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

  def copy(schema: Schema, name: String, path: List[String], description: Option[String]): Schema = schema match {
    case s: ObjectSchema =>
      s.copy(name = name, path = path, description = description.orElse(schema.description))
    case s: ArraySchema =>
      s.copy(name = name, path = path, description = description.orElse(schema.description))
    case s: MapSchema =>
      s.copy(name = name, path = path, description = description.orElse(schema.description))
    case s: OneOfSchema =>
      s.copy(name = name, path = path, description = description.orElse(schema.description))
    case s: StringSchema =>
      s.copy(name = name, path = path, description = description.orElse(schema.description))
    case s: NumberSchema =>
      s.copy(name = name, path = path, description = description.orElse(schema.description))
    case s: IntegerSchema =>
      s.copy(name = name, path = path, description = description.orElse(schema.description))
    case s: BooleanSchema =>
      s.copy(name = name, path = path, description = description.orElse(schema.description))
    case s: NullSchema =>
      s.copy(name = name, path = path, description = description.orElse(schema.description))
    case s: InternalSchemaReference =>
      s.copy(name = name, path = path, description = description.orElse(schema.description))
    case s: ExternalSchemaReference =>
      s.copy(name = name, path = path, description = description.orElse(schema.description))
  }
}
