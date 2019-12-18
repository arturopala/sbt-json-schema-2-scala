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

trait Code {
  def append(b: CodeSink): Unit
}

object Code {
  def toString(code: Seq[Code]): String = {
    val b = new StringBuilderCodeSink
    code.foreach(c => {
      c.append(b)
      b.newline
    })
    b.toString
  }
}

trait CodeSink {
  protected var indentSize = 0
  protected val indent = "  "

  def append(s: String): CodeSink
  def newline: CodeSink = append("\n").append(indent * indentSize)
  def indentInc: CodeSink = { indentSize = indentSize + 1; this }
  def indentDec: CodeSink = { indentSize = Math.max(0, indentSize - 1); this }
}

class StringBuilderCodeSink extends CodeSink {
  val sb = new StringBuilder
  override def append(s: String): CodeSink = { sb.append(s); this }
  override def toString: String = sb.toString()
}
