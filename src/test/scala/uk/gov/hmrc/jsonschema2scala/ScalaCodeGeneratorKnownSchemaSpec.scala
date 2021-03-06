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
import uk.gov.hmrc.jsonschema2scala.schema.{DebugOptions, SchemaSource}

class ScalaCodeGeneratorKnownSchemaSpec
    extends WordSpec with Matchers with CodeRenderingAssertions with TestSchemas with BeforeAndAfterAll {

  implicit val compiler: Compiler = Compiler()
  implicit val debug: DebugOptions = DebugOptions()

  override def afterAll(): Unit =
    compiler.cleanup()

  "Generate from known schemas" should {

    verifiedTestSchemas
      .foreach { schema: SchemaSource =>
        s"generate from ${schema.name}" in assertCanParseAndCompile(schema, allSchemas)
      }
  }

}
