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

import uk.gov.hmrc.jsonschema2scala.schema.{SchemaResource, SchemaSource}

import scala.util.Try

trait TestSchemas {

  val testSchemas: Seq[SchemaSource] = (1 to 20)
    .map { no =>
      Try(classOf[SchemaSpec].getResourceAsStream(f"/schemas/E$no%02d.schema.json")).map {
        SchemaResource(_, f"Entity$no%02d")
      }.toOption
    }
    .collect { case Some(x) => x }

  val testReferences: Map[String, SchemaSource] = testSchemas.map(s => (s.uri.toString, s)).toMap

}
