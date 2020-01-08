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
import uk.gov.hmrc.jsonschema2scala.schema.SchemaSource

class ScalaCodeGeneratorSpec
    extends WordSpec with Matchers with CodeRenderingAssertions with TestSchemas with BeforeAndAfterAll {

  implicit val compiler: Compiler = Compiler()

  override def afterAll(): Unit =
    compiler.cleanup()

  "JsonSchema2ScalaCodeRenderer" should {
    "fail generating from simple schema of primitive type" in
      assertRenderingFails("""
                             |{
                             |  "$id": "http://example.com/test.json",
                             |  "description": "A test schema",
                             |  "type": "string"
                             |}
                               """.stripMargin)

    "generate from simple schema into a case class without body and no companion object" in
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

    "generate from simple schema with cyclic self-reference" in
      assertCanParseAndCompile("""
                                 |{
                                 |  "$id": "http://example.com/test.json",
                                 |  "description": "A test schema",
                                 |  "type": "object",
                                 |  "properties": {
                                 |    "one": {
                                 |      "$ref": "#/definitions/one"
                                 |    }
                                 |  },
                                 |  "required": [ "one" ],
                                 |  "definitions": {
                                 |    "one": {
                                 |      "$ref": "http://example.com/test.json"
                                 |    }
                                 |  }
                                 |}
                               """.stripMargin)

    "generate from schema having deeply nested objects" in
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
                                 |      "type": "object",
                                 |      "properties": {
                                 |        "one": {
                                 |          "type": "string"
                                 |        },
                                 |        "second": {
                                 |          "type": "integer"
                                 |        },
                                 |        "third": {
                                 |          "type": "object",
                                 |          "properties": {
                                 |            "one": {
                                 |              "type": "object",
                                 |              "properties": {
                                 |                "second" : {
                                 |                  "type": "array",
                                 |                  "items": {
                                 |                    "type": "object",
                                 |                    "properties": {
                                 |                      "one": {
                                 |                        "type": "string"
                                 |                      },
                                 |                      "third": {
                                 |                        "type": "number"
                                 |                      }
                                 |                    },
                                 |                    "required": ["third"]
                                 |                  }
                                 |                }
                                 |              }
                                 |            },
                                 |            "second": {
                                 |             "type": "integer"
                                 |            },
                                 |            "third": {
                                 |              "type": "boolean"
                                 |            }
                                 |          },
                                 |          "required": [ "second", "third" ]
                                 |        }
                                 |      }
                                 |    },
                                 |    "third": {
                                 |      "type": "string"
                                 |    }
                                 |  },
                                 |  "required": [ "one", "third" ]
                                 |}
                               """.stripMargin)

    "generate from simple schema of an object having array of primitives" in
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

    "generate from simple schema of an object having array of objects" in
      assertCanParseAndCompile("""
                                 |{
                                 |    "$id": "http://example.com/test.json",
                                 |    "description": "A test schema",
                                 |    "type": "object",
                                 |    "properties": {
                                 |        "one": {
                                 |            "type": "string"
                                 |        },
                                 |        "secondArray": {
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

    "generate from simple schema containing oneOf alternative primitive values" in
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
                                 |      "oneOf":[
                                 |         {
                                 |           "type": "string",
                                 |           "const": "foo"
                                 |         },
                                 |         {
                                 |           "type": "string",
                                 |           "const": "foo"
                                 |         }
                                 |      ]
                                 |    },
                                 |    "thi-rd": {
                                 |      "type": "boolean"
                                 |    }
                                 |  },
                                 |  "required": [ "second" ]
                                 |}
                               """.stripMargin)

    "generate from simple object schema containing oneOf alternative object values" in
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
                                 |             },
                                 |             "foo": {
                                 |               "type": "integer"
                                 |             }
                                 |           }
                                 |         },
                                 |         {
                                 |           "type": "object",
                                 |           "properties": {
                                 |             "ver": {
                                 |               "type": "number"
                                 |             },
                                 |             "foo": {
                                 |               "type": "integer"
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

    "generate from top level oneOf schema" in
      assertCanParseAndCompile("""{
                                 |  "$id": "http://example.com/test.json",
                                 |  "description": "A test schema",
                                 |  "oneOf": [
                                 |    {
                                 |      "description": "First option",
                                 |      "$ref": "#/$defs/one"
                                 |    },
                                 |    {
                                 |      "description": "Second option",
                                 |      "properties": {
                                 |        "foo": {
                                 |          "type": "integer"
                                 |        },
                                 |        "ver": {
                                 |          "type": "number"
                                 |        },
                                 |        "ref": {
                                 |          "$ref": "#/$defs/one"
                                 |        }
                                 |      }
                                 |    }
                                 |  ],
                                 |  "$defs": {
                                 |    "one": {
                                 |      "properties": {
                                 |        "foo": {
                                 |          "type": "integer"
                                 |        },
                                 |        "ver": {
                                 |          "type": "string"
                                 |        }
                                 |      }
                                 |    }
                                 |  }
                                 |}""".stripMargin)

    "generate from simple schema containing oneOf alternative object and primitive values" in
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

    "generate from schema with internal references" in
      assertCanParseAndCompile("""{
                                 |  "$id": "http://example.com/test.json",
                                 |  "description": "A test schema",
                                 |  "type": "object",
                                 |  "properties": {
                                 |    "one": {
                                 |      "$ref": "#/properties/second/definitions/one"
                                 |    },
                                 |    "second": {
                                 |      "type": "object",
                                 |      "properties": {
                                 |        "one": {
                                 |          "type": "string"
                                 |        },
                                 |        "second": {
                                 |          "type": "integer"
                                 |        },
                                 |        "third": {
                                 |          "$ref": "#/definitions/foo"
                                 |        }
                                 |      },
                                 |      "definitions": {
                                 |        "one": {
                                 |          "type": "object",
                                 |          "properties": {
                                 |            "one": {
                                 |              "type": "string"
                                 |            }
                                 |          }
                                 |        }
                                 |      }
                                 |    },
                                 |    "fourth": {
                                 |      "$ref": "#/definitions/foo"
                                 |    }
                                 |  },
                                 |  "required": [
                                 |    "one",
                                 |    "fourth"
                                 |  ],
                                 |  "definitions": {
                                 |    "foo": {
                                 |      "type": "object",
                                 |      "properties": {
                                 |        "one": {
                                 |          "type": "object",
                                 |          "properties": {
                                 |            "one": {
                                 |              "type": "integer"
                                 |            },
                                 |            "second": {
                                 |              "type": "array",
                                 |              "items": {
                                 |                "type": "object",
                                 |                "properties": {
                                 |                  "one": {
                                 |                    "$ref": "#/properties/second/definitions/one"
                                 |                  },
                                 |                  "third": {
                                 |                    "type": "number"
                                 |                  }
                                 |                },
                                 |                "required": [
                                 |                  "third"
                                 |                ]
                                 |              }
                                 |            }
                                 |          }
                                 |        },
                                 |        "second": {
                                 |          "type": "integer"
                                 |        },
                                 |        "third": {
                                 |          "type": "boolean"
                                 |        }
                                 |      },
                                 |      "required": [
                                 |        "second",
                                 |        "third"
                                 |      ]
                                 |    }
                                 |  }
                                 |}""".stripMargin)

    "generate from schema with any of: nullable item or an array of item" in
      assertCanParseAndCompile("""{
                                 |  "$id": "http://example.com/test.json",
                                 |  "definitions": {
                                 |    "Item": {
                                 |      "additionalProperties": {
                                 |        "anyOf": [
                                 |          {
                                 |            "additionalProperties": {},
                                 |            "type": "object"
                                 |          },
                                 |          {
                                 |            "type": "null"
                                 |          }
                                 |        ]
                                 |      }
                                 |    }
                                 |  },
                                 |  "description": "A test schema",
                                 |  "properties": {
                                 |    "Item": {
                                 |      "anyOf": [
                                 |        {
                                 |          "$ref": "#/definitions/Item"
                                 |        },
                                 |        {
                                 |          "items": {
                                 |            "anyOf": [
                                 |              {
                                 |                "$ref": "#/definitions/Item"
                                 |              },
                                 |              {
                                 |                "type": "string"
                                 |              }
                                 |            ]
                                 |          },
                                 |          "type": "array"
                                 |        },
                                 |        {
                                 |          "type": "null"
                                 |        }
                                 |      ]
                                 |    }
                                 |  },
                                 |  "type": "object"
                                 |}""".stripMargin)

    verifiedTestSchemas /*.filter(_.name == "jscsrc.json")*/.foreach { schema: SchemaSource =>
      s"generate from ${schema.name}" in assertCanParseAndCompile(schema, verifiedTestSchemas)
    }
  }

}
