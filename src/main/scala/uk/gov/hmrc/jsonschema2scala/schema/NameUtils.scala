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

package uk.gov.hmrc.jsonschema2scala.schema

import java.util.regex.Pattern

object NameUtils {

  final val NORMALIZE_PATTERN = Pattern.compile("""([_/@:$-]+?\w)""")

  final def normalize(text: String): String = {
    val m = NORMALIZE_PATTERN.matcher(text)
    val sb = new StringBuilder
    var last = 0
    while (m.find) {
      sb.append(text.substring(last, m.start))
      sb.append(m.group(0).replaceAll("[_/@:$-]", "").toUpperCase)
      last = m.end
    }
    sb.append(text.substring(last))
    sb.toString
  }

  final def firstCharUppercase(text: String): String =
    if (text.isEmpty) text
    else text.substring(0, 1).toUpperCase + text.substring(1)

  final def firstCharLowercase(text: String): String =
    if (text.isEmpty) text
    else text.substring(0, 1).toLowerCase + text.substring(1)

  final def variableName(definition: Schema): String =
    firstCharLowercase(normalize(definition.name))

  final def pathLastPart(definition: Schema): String = definition.path.head

  final def pathToName(definition: Schema): String = {
    val name = pathLastPart(definition)
    if (name.endsWith("Type")) name.dropRight(4) else name
  }

  final def singular(name: String): String =
    if (name.endsWith("Array")) name.substring(0, name.length - 5)
    else if (name.endsWith("List")) name.substring(0, name.length - 4)
    else name
}
