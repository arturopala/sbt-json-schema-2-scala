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
import uk.gov.hmrc.jsonschema2scala.generator.TypeResolver.shortenPrefix

class TypeResolverSpec extends WordSpec with Matchers with TestSchemas {

  "TypeNameResolver" should {
    "strip a common prefix from the path" in {
      shortenPrefix(Nil, Nil) shouldBe Nil
      shortenPrefix(Nil, "a" :: Nil) shouldBe Nil
      shortenPrefix(Nil, "b" :: "a" :: Nil) shouldBe Nil
      shortenPrefix("a" :: Nil, Nil) shouldBe "a" :: Nil
      shortenPrefix("b" :: "a" :: Nil, Nil) shouldBe "b" :: "a" :: Nil
      shortenPrefix("b" :: "a" :: Nil, "b" :: Nil) shouldBe "b" :: "a" :: Nil
      shortenPrefix("b" :: "a" :: Nil, "a" :: Nil) shouldBe "b" :: "a" :: Nil
      shortenPrefix("c" :: "b" :: "a" :: Nil, "b" :: "a" :: Nil) shouldBe "c" :: "b" :: "a" :: Nil
      shortenPrefix("c" :: "b" :: "a" :: Nil, "c" :: "b" :: "a" :: Nil) shouldBe "a" :: Nil
      shortenPrefix("c" :: "b" :: "a" :: Nil, "d" :: "b" :: "a" :: Nil) shouldBe "c" :: "b" :: "a" :: Nil
      shortenPrefix("c" :: "b" :: "d" :: Nil, "c" :: "b" :: "a" :: Nil) shouldBe "b" :: "d" :: Nil
      shortenPrefix("c" :: "b" :: "a" :: Nil, "c" :: "d" :: "a" :: Nil) shouldBe "c" :: "b" :: "a" :: Nil
      shortenPrefix("e" :: "c" :: "b" :: "a" :: Nil, "e" :: "c" :: "d" :: "a" :: Nil) shouldBe "c" :: "b" :: "a" :: Nil
    }
  }

}
