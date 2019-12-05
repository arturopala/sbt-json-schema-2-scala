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

import org.scalatest.{Matchers, WordSpec}
import uk.gov.hmrc.jsonschema2scala.JsonSchema.{Schema, SchemaResource}

class JsonSchemaSpec extends WordSpec with Matchers {

  val schemas: Seq[Schema] = (1 to 16).map(no =>
    SchemaResource(classOf[JsonSchemaSpec].getResourceAsStream(f"/schemas/E$no%02d.schema.json"), f"Entity$no%02d"))

  val references: Map[String, Schema] = schemas.map(s => (s.id.getOrElse("unknown"), s)).toMap

  "JsonSchema" should {
    schemas.foreach { schema =>
      s"read and parse ${schema.className} schema" in {
        val json = schema.body
        val definition = JsonSchema.read(json, references)
        definition.isPrimitive shouldBe false
      }
    }
  }

}
