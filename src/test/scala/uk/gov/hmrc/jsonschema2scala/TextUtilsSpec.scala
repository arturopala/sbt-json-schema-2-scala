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
import uk.gov.hmrc.jsonschema2scala.generator.TextUtils
import uk.gov.hmrc.jsonschema2scala.generator.TextUtils.splitAndNormalize

class TextUtilsSpec extends WordSpec with Matchers with TestSchemas {

  val loremIpsum =
    "Lorem ipsum dolor sit amet, consectetur adipisci elit. Sed, do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

  "TextUtils" should {
    "split text to fit around expected line length" in {
      splitAndNormalize(loremIpsum, 15, TextUtils.findCommentSplitPosition).drop(1).head shouldBe "Lorem ipsum dolor "
      splitAndNormalize(loremIpsum, 20, TextUtils.findCommentSplitPosition)
        .drop(1)
        .head shouldBe "Lorem ipsum dolor sit amet,"
      splitAndNormalize(loremIpsum, 30, TextUtils.findCommentSplitPosition)
        .drop(1)
        .head shouldBe "Lorem ipsum dolor sit amet,"
      splitAndNormalize(loremIpsum, 60, TextUtils.findCommentSplitPosition)
        .drop(1)
        .head shouldBe "Lorem ipsum dolor sit amet, consectetur adipisci elit."
      splitAndNormalize("Lorem ipsum dolor sit amet, consectetur adipisci elit", 45, TextUtils.findCommentSplitPosition).headOption shouldBe Some(
        "Lorem ipsum dolor sit amet, consectetur adipisci elit")
    }
  }

}
