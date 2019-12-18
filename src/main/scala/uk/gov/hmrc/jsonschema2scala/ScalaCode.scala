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

sealed trait ScalaCode extends Code

object ScalaCode {

  case class CaseClass(
    name: String,
    parameters: Seq[Param],
    supertypes: Seq[String],
    members: Seq[ScalaCode],
    comment: Option[String] = None)
      extends ScalaCode {
    override def append(b: Sink): Unit = {
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

    override def append(b: Sink): Unit = {
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
    supertypes: Seq[String],
    members: Seq[ScalaCode],
    modifier: Option[String] = None,
    comment: Option[String] = None)
      extends ScalaCode {
    override def append(b: Sink): Unit = {
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
    override def append(b: Sink): Unit = {
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
    override def append(b: Sink): Unit = {
      b.append("package ")
      b.append(name)
      b.newline
    }
  }

  case class Import(packageName: String, typeNames: List[String]) extends ScalaCode {
    override def append(b: Sink): Unit =
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
    override def append(b: Sink): Unit = {
      b.append("import ")
      b.append(name)
      b.append("._")
    }
  }

  case class BlockComment(comment: String, doc: Boolean = true) extends ScalaCode {
    override def append(sink: Sink): Unit = BlockComment.append(comment, sink, doc)
  }

  object BlockComment {
    def append(comment: String, b: Sink, doc: Boolean): Unit = {
      val lines = splitAndNormalize(comment, 80)
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

    def splitAndNormalize(comment: String, count: Int): List[String] =
      comment
        .split('\n')
        .foldLeft(List.empty[String])(
          (acc, v) =>
            if (v.trim.nonEmpty) v :: acc
            else
              acc match {
                case Nil                       => v :: Nil
                case x :: xs if x.trim.isEmpty => xs
                case _                         => v :: acc
            })
        .reverse
        .flatMap(splitAround(_, count)) match {
        case Nil     => Nil
        case x :: xs => if (xs.isEmpty || x.trim.isEmpty) x :: xs else "" :: x :: xs
      }

    def splitAround(string: String, count: Int): List[String] =
      if (string.length < count) List(string)
      else {
        val p = findSplitPosition(string, count)
        if (p < 0) List(string)
        else {
          val slice = string.substring(0, p + 1)
          val next = {
            val s = string.substring(p + 1)
            if ((slice.endsWith(",") || slice.endsWith(".")) && s.startsWith(" ")) s.substring(1) else s
          }
          slice :: splitAround(next, count)
        }
      }

    def findSplitPosition(string: String, count: Int): Int = {
      val positions = List(
        (string.indexOf('.', Math.max(count - 10, 0)), 0.5, 0),
        (string.indexOf('.', count), 0.5, 1),
        (string.indexOf(',', Math.max(count - 10, 0)), 1.0, 4),
        (string.indexOf(',', count), 1.0, 5),
        (string.indexOf(' ', Math.max(count - 10, 0)), 2.0, 8),
        (string.indexOf(' ', count), 2.0, 9)
      ).filter(_._1 >= 0).map {
        case (pos, weight, offset) =>
          (pos, Math.abs(count - pos) * weight + offset)
      }
      positions match {
        case Nil      => -1
        case nonEmpty => nonEmpty.minBy(_._2)._1
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
    override def append(b: Sink): Unit = {
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
    override def append(b: Sink): Unit = {
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
