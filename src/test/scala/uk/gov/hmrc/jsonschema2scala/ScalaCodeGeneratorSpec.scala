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

class ScalaCodeGeneratorSpec
    extends WordSpec with Matchers with CodeRenderingAssertions with TestSchemas with BeforeAndAfterAll {

  implicit val compiler: Compiler = Compiler()

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

    "render a schema having deeply nested objects" in
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

    "render a simple object schema containing oneOf alternative object values" in
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

    "render a top level oneOf schema" in
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

    "render a schema with internal references" in
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
  }

}
