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
import uk.gov.hmrc.jsonschema2scala.schema.{DebugOptions, SchemaReader, SchemaReferenceResolver}

class SchemaReaderSpec extends WordSpec with Matchers with TestSchemas {

  val debug: DebugOptions = DebugOptions()

  "SchemaReader" should {
    val multiResolver = SchemaReferenceResolver(allSchemas)
    allSchemas
    //.filter(_.name == "compilerconfig.json")
      .foreach { schemaSource =>
        s"read ${schemaSource.name} schema" in {
          val resolver = SchemaReferenceResolver(schemaSource, Some(multiResolver))
          val definition = SchemaReader.read(schemaSource, resolver, debug)
          definition.primitive shouldBe false
        }
      }
  }

}
