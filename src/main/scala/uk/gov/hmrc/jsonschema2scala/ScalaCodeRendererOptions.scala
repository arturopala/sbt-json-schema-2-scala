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

package uk.gov.hmrc.jsonschema2scala

sealed trait JsonSchema2ScalaFeature

object JsonSchema2ScalaFeature {
  case object Validator extends JsonSchema2ScalaFeature
  case object Generator extends JsonSchema2ScalaFeature
  case object Sanitizer extends JsonSchema2ScalaFeature
  case object Builder extends JsonSchema2ScalaFeature
  case object PlayJson extends JsonSchema2ScalaFeature

  val ALL: Set[JsonSchema2ScalaFeature] = Set(Builder, PlayJson, Validator, Generator, Sanitizer)
}

case class ScalaCodeRendererOptions(features: Set[JsonSchema2ScalaFeature], packageName: String)