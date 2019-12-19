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

package uk.gov.hmrc.jsonschema2scala.generator.scala2

import uk.gov.hmrc.jsonschema2scala.generator.{Code, CodeSink, TextUtils}

sealed trait ScalaCode extends Code

object ScalaCode {

  case class CaseClass(
    name: String,
    parameters: Seq[Param],
    supertypes: Seq[String],
    members: Seq[ScalaCode],
    comment: Option[String] = None)
      extends ScalaCode {
    override def append(b: CodeSink): Unit = {
      comment.foreach { c =>
        BlockComment.append(c, b, doc = true)
        b.newline
      }
      b.append("case class ")
      b.append(name)
      b.append("(")
      if (parameters.nonEmpty) {
        b.indentInc.newline
        parameters.head.append(b)
        parameters.tail.foreach(p => {
          b.append(",")
          b.newline
          p.append(b)
        })
        b.indentDec.newline
      }
      b.append(")")
      if (supertypes.nonEmpty) {
        b.append(" extends ")
        b.append(supertypes.head)
        supertypes.tail.foreach(typeName => {
          b.append(" with ")
          b.append(typeName)
        })
      }
      if (members.nonEmpty) {
        b.append("{")
        b.indentInc
        members.foreach(member => {
          b.newline
          member.append(b)
        })
        b.indentDec.newline
        b.append("}")
      }
      b.newline
    }
  }

  case class Object(name: String, supertypes: Seq[String], members: Seq[ScalaCode]) extends ScalaCode {

    override def append(b: CodeSink): Unit = {
      b.append("object ")
      b.append(name)
      b.append(" {")
      b.indentInc
      members.foreach(m => {
        b.newline
        m.append(b)
      })
      b.indentDec.newline
      b.append("}")
      b.newline
    }

    def asOption: Option[Object] = if (members.isEmpty) None else Some(this)
  }

  case class Trait(
    name: String,
    supertypes: Seq[String] = Seq.empty,
    members: Seq[ScalaCode] = Seq.empty,
    modifier: Option[String] = None,
    comment: Option[String] = None)
      extends ScalaCode {
    override def append(b: CodeSink): Unit = {
      comment.foreach { c =>
        BlockComment.append(c, b, doc = true)
        b.newline
      }
      modifier.foreach(m => {
        b.append(m)
        b.append(" ")
      })
      b.append("trait ")
      b.append(name)
      if (supertypes.nonEmpty) {
        b.append(" extends ")
        b.append(supertypes.head)
        supertypes.tail.foreach(typeName => {
          b.append(" with ")
          b.append(typeName)
        })
      }
      if (members.nonEmpty) {
        b.append(" {")
        b.indentInc
        members.foreach(m => {
          b.newline
          m.append(b)
        })
        b.indentDec.newline
        b.append("}")
      }
      b.newline
    }
  }

  case class Param(name: String, typeName: String, modifier: Option[String] = None, comment: Option[String] = None)
      extends ScalaCode {
    override def append(b: CodeSink): Unit = {
      comment.foreach { c =>
        BlockComment.append(c, b, doc = false)
        b.newline
      }
      modifier.foreach(m => {
        b.append(m)
        b.append(" ")
      })
      b.append(name)
      b.append(": ")
      b.append(typeName)
    }
  }

  case class Package(name: String) extends ScalaCode {
    override def append(b: CodeSink): Unit = {
      b.append("package ")
      b.append(name)
      b.newline
    }
  }

  case class Import(packageName: String, typeNames: List[String]) extends ScalaCode {
    override def append(b: CodeSink): Unit =
      typeNames match {
        case Nil => Seq()
        case head :: tail =>
          b.append("import ")
          b.append(packageName)
          b.append(".")
          if (tail.isEmpty) {
            b.append(head)
          } else {
            b.append("{")
            b.append(head)
            tail.foreach(typeName => {
              b.append(", ")
              b.append(typeName)
            })
            b.append(" }")
          }
      }
  }

  case class WildcardImport(name: String) extends ScalaCode {
    override def append(b: CodeSink): Unit = {
      b.append("import ")
      b.append(name)
      b.append("._")
    }
  }

  case class BlockComment(comment: String, doc: Boolean = true) extends ScalaCode {
    override def append(sink: CodeSink): Unit = BlockComment.append(comment, sink, doc)
  }

  object BlockComment {
    def append(comment: String, b: CodeSink, doc: Boolean): Unit = {
      val lines = TextUtils.splitAndNormalize(comment, 80)
      if (lines.nonEmpty) {
        lines.headOption.map(line => {
          b.append(if (doc) "/** " else "/* ")
          b.append(line)
        })
        lines.tail.foreach(line => {
          b.newline
          b.append(if (doc) "  * " else " * ")
          b.append(line)
        })
        if (lines.length > 1) b.newline
        b.append(if (doc) "  */" else " */")
      }
    }
  }

  case class MethodDefinition(
    name: String,
    parameters: Seq[Param],
    returnType: String,
    body: Seq[String],
    modifier: Option[String] = None)
      extends ScalaCode {
    override def append(b: CodeSink): Unit = {
      modifier.foreach(m => {
        b.append(m)
        b.append(" ")
      })
      b.append("def ")
      b.append(name)
      if (parameters.nonEmpty) {
        b.append("(")
        parameters.head.append(b)
        parameters.tail.foreach(p => {
          b.append(", ")
          p.append(b)
        })
        b.append(")")
      }
      b.append(": ")
      b.append(returnType)
      if (body.nonEmpty) {
        b.append(" = ")
        if (body.size > 1) {
          b.append("{")
          b.indentInc
        }
        body.foreach(c => {
          if (body.size > 1) {
            b.newline
          }
          b.append(c)
        })
        if (body.size > 1) {
          b.indentDec.newline
          b.append("}")
        }
      }
    }
  }

  case class ValueDefinition(name: String, returnType: String, body: Seq[String], modifier: Option[String] = None)
      extends ScalaCode {
    override def append(b: CodeSink): Unit = {
      modifier.foreach(m => {
        b.append(m)
        b.append(" ")
      })
      b.append("val ")
      b.append(name)
      Option(returnType).foreach(rt => {
        b.append(": ")
        b.append(rt)
      })
      if (body.nonEmpty) {
        b.append(" = ")
        if (body.size > 1) {
          b.append("{")
          b.indentInc
        }
        body.foreach(c => {
          if (body.size > 1) {
            b.newline
          }
          b.append(c)
        })
        if (body.size > 1) {
          b.indentDec.newline
          b.append("}")
        }
      }
    }
  }

}
