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

import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}

class ScalaCodeRendererSpec
    extends WordSpec with Matchers with CodeRenderingAssertions with TestSchemas with BeforeAndAfterAll {

  implicit val compiler = Compiler()

  override def afterAll(): Unit =
    compiler.cleanup()

  "JsonSchema2ScalaCodeRenderer" should {
    "fail rendering simple schema of primitive type" in
      assertRenderingFails("""
                             |{
                             |  "$id": "http://example.com/test.json",
                             |  "description": "A test schema",
                             |  "type": "string"
                             |}
                               """.stripMargin)

    "render a simple schema into a case class without body and no companion object" in
      assertCanParseAndCompile("""
                                 |{
                                 |  "$id": "http://example.com/test.json",
                                 |  "description": "A test schema",
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
                             """.stripMargin)

    "render a simple schema of an object having array of primitives" in
      assertCanParseAndCompile("""
                                 |{
                                 |  "$id": "http://example.com/test.json",
                                 |  "description": "A test schema",
                                 |  "type": "object",
                                 |  "properties": {
                                 |    "one": {
                                 |      "type": "string"
                                 |    },
                                 |    "second": {
                                 |      "type": "array",
                                 |      "minItems":1,
                                 |			"items": {
                                 |        "type": "string"
                                 |      }
                                 |    }
                                 |  },
                                 |  "required": [ "one" ]
                                 |}
                               """.stripMargin)

    "render a simple schema of an object having array of objects" in
      assertCanParseAndCompile("""
                                 |{
                                 |    "$id": "http://example.com/test.json",
                                 |    "description": "A test schema",
                                 |    "type": "object",
                                 |    "properties": {
                                 |        "one": {
                                 |            "type": "string"
                                 |        },
                                 |        "second": {
                                 |            "type": "array",
                                 |            "minItems": 1,
                                 |            "items": {
                                 |                "type": "object",
                                 |                "properties": {
                                 |                    "one": {
                                 |                        "type": "number"
                                 |                    },
                                 |                    "second": {
                                 |                        "type": "boolean"
                                 |                    }
                                 |                },
                                 |                "required": ["second"]
                                 |            }
                                 |        }
                                 |    },
                                 |    "required": ["one"]
                                 |}
                                 |""".stripMargin)

    "render a simple schema containing oneOf alternative primitive values" in
      assertCanParseAndCompile("""
                                 |{
                                 |  "$id": "http://example.com/test.json",
                                 |  "description": "A test schema",
                                 |  "type": "object",
                                 |  "properties": {
                                 |    "one": {
                                 |      "oneOf":[
                                 |         {
                                 |           "type": "string"
                                 |         },
                                 |         {
                                 |           "type": "number"
                                 |         }
                                 |      ]
                                 |    },
                                 |    "second": {
                                 |      "type": "number"
                                 |    },
                                 |    "thi-rd": {
                                 |      "type": "boolean"
                                 |    }
                                 |  },
                                 |  "required": [ "second" ]
                                 |}
                               """.stripMargin)

    "render a simple schema containing oneOf alternative object values" in
      assertCanParseAndCompile("""
                                 |{
                                 |  "$id": "http://example.com/test.json",
                                 |  "description": "A test schema",
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
                               """.stripMargin)

    "render a simple schema containing oneOf alternative object and primitive values" in
      assertCanParseAndCompile("""
                                 |{
                                 |  "$id": "http://example.com/test.json",
                                 |  "description": "A test schema",
                                 |  "type": "object",
                                 |  "properties": {
                                 |    "one": {
                                 |      "oneOf":[
                                 |         {
                                 |           "type": "string"
                                 |         },
                                 |         {
                                 |           "type": "object",
                                 |           "properties": {
                                 |              "one": {
                                 |                "type": "number"
                                 |              }
                                 |           }
                                 |         }
                                 |      ]
                                 |    },
                                 |    "second": {
                                 |      "type": "number"
                                 |    },
                                 |    "thi-rd": {
                                 |      "type": "boolean"
                                 |    }
                                 |  },
                                 |  "required": [ "one" ]
                                 |}
                               """.stripMargin)

    testSchemas.foreach { schema: JsonSchema.Schema =>
      s"render ${schema.className} schema" in assertCanParseAndCompile(schema, testReferences)
    }
  }

}
