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

package uk.gov.hmrc.jsonschema2scala.generator

import uk.gov.hmrc.jsonschema2scala.schema.Schema
import uk.gov.hmrc.jsonschema2scala.typer.TypeDefinition

import scala.annotation.tailrec

trait TypeResolver {

  def typeOf(
    schema: Schema,
    viewpoint: TypeDefinition,
    wrapAsOption: Boolean = true,
    showDefaultValue: Boolean = true): String

  def interfacesOf(schema: Schema, viewpoint: TypeDefinition): Set[String]

}

object TypeResolver {

  @tailrec
  final def shortenPrefix(path: List[String], otherPath: List[String], previous: Option[String] = None): List[String] =
    path match {
      case Nil => previous.map(_ :: path).getOrElse(path)
      case pathHead :: pathTail =>
        otherPath match {
          case Nil => previous.map(_ :: path).getOrElse(path)
          case otherHead :: otherTail =>
            if (pathHead != otherHead) previous.map(_ :: path).getOrElse(path)
            else shortenPrefix(pathTail, otherTail, Some(pathHead))
        }
    }

}
