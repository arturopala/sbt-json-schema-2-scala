/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.jsonschema2scala.typer

import java.security.MessageDigest

object SHA256 {

  val digest: MessageDigest = MessageDigest.getInstance("SHA-256")

  final def hashOf(string: String, numberOfBytes: Int = 32): String = {
    val bytes = string.getBytes("UTF-8")
    digest
      .digest(bytes)
      .take(numberOfBytes)
      .map(b => Integer.toHexString(0xff & b))
      .mkString
  }

}
