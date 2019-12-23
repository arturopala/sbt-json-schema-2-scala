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

package uk.gov.hmrc.jsonschema2scala.typer

import uk.gov.hmrc.jsonschema2scala.schema.NameUtils.{firstCharUppercase, normalize}
import uk.gov.hmrc.jsonschema2scala.schema.{ExternalSchemaReference, InternalSchemaReference, Schema}

object ScalaTypeNameProvider extends TypeNameProvider {

  override def toTypeName(schema: Schema): String = {
    val name = schema match {
      case i: InternalSchemaReference =>
        i.schema.name
      case e: ExternalSchemaReference =>
        e.schema.name
      case s => s.name
    }
    firstCharUppercase(
      normalize(
        if (name.endsWith(".schema.json")) name.dropRight(12)
        else if (name.endsWith(".json")) name.dropRight(5)
        else name))
  }

  override def toIdentifier(name: String): String =
    if (name.exists(noNameChars.contains) || scalaKeywords.contains(name) || Character.isDigit(name.charAt(0)))
      s"`$name`"
    else if (objectMemberNames.contains(name)) s"__$name"
    else name

  override def toTypeNameVariant(schema: Schema, pos: Int): String =
    s"${toTypeName(schema)}_$pos"

  override def toTypePatternName(schema: Schema): String =
    "Pattern_" + SHA256.hashOf(schema.name, numberOfBytes = 4)

  final val scalaKeywords: Set[String] = Set(
    "abstract",
    "case",
    "catch",
    "class",
    "def",
    "do",
    "else",
    "extends",
    "false",
    "final",
    "finally",
    "for",
    "forSome",
    "if",
    "implicit",
    "import",
    "lazy",
    "match",
    "new",
    "Null",
    "object",
    "override",
    "package",
    "private",
    "protected",
    "return",
    "sealed",
    "super",
    "this",
    "throw",
    "trait",
    "try",
    "true",
    "type",
    "val",
    "var",
    "while",
    "with",
    "yield"
  )

  final val objectMemberNames: Seq[String] =
    Seq("wait", "notify", "notifyAll", "finalize", "equals", "finalize", "clone", "hashCode")

  final val noNameChars: Set[Char] = Set('@', '$', '-', '/', '.')
}
