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
import play.api.libs.json.{JsObject, Json}

class ScalaCodeRendererSpec extends WordSpec with Matchers {

  "JsonSchema2ScalaCodeRenderer" should {
    "process simple schema into a case class without body and no companion object" in {
      val options = JsonSchema2ScalaOptions(features = Set(), packageName = "a.b.c")
      val schemaJson = Json.parse("""
                                    |{
                                    |  "$id": "http://example.com/test.json",
                                    |  "description": "An test schema",
                                    |  "type": "object",
                                    |  "properties": {
                                    |    "aba": {
                                    |      "type": "string"
                                    |    },
                                    |    "eg-dhj": {
                                    |      "type": "number"
                                    |    },
                                    |    "abc-def": {
                                    |      "type": "boolean"
                                    |    }
                                    |  },
                                    |  "required": [ "aba" ]
                                    |}
        """.stripMargin).as[JsObject]
      val definition = JsonSchema.read(schemaJson)
      val code = JsonSchema2ScalaCodeRenderer.render("Test", definition, options, "")
      Compiler.compileCode(code, show = true)
    }

    "process simple schema containing oneOf values" in {
      val options = JsonSchema2ScalaOptions(features = Set(), packageName = "a.b.c")
      val schemaJson = Json.parse("""
                                    |{
                                    |  "$id": "http://example.com/test.json",
                                    |  "description": "An test schema",
                                    |  "type": "object",
                                    |  "properties": {
                                    |    "aba": {
                                    |      "oneOf":[
                                    |         {
                                    |           "type": "object",
                                    |           "properties": {
                                    |             "ver": {
                                    |               "type": "string"
                                    |             }
                                    |           }
                                    |         },
                                    |         {
                                    |           "type": "object",
                                    |           "properties": {
                                    |             "ver": {
                                    |               "type": "number"
                                    |             }
                                    |           }
                                    |         }
                                    |      ]
                                    |    },
                                    |    "eg-dhj": {
                                    |      "type": "number"
                                    |    },
                                    |    "abc-def": {
                                    |      "type": "boolean"
                                    |    }
                                    |  },
                                    |  "required": [ "aba" ]
                                    |}
        """.stripMargin).as[JsObject]
      val definition = JsonSchema.read(schemaJson)
      val code = JsonSchema2ScalaCodeRenderer.render("Test", definition, options, "")
      Compiler.compileCode(code, show = true)
    }
  }

}
