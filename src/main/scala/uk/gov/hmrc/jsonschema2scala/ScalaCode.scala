package uk.gov.hmrc.jsonschema2scala

sealed trait ScalaCode extends Code

object ScalaCode {

  case class CaseClass(name: String, parameters: Seq[Param], supertypes: Seq[String], members: Seq[ScalaCode])
      extends ScalaCode {
    override def append(b: Sink): Unit = {
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
    }

    def asOption: Option[Object] = if (members.isEmpty) None else Some(this)
  }

  case class Trait(name: String, supertypes: Seq[String], members: Seq[ScalaCode], modifier: Option[String] = None)
      extends ScalaCode {
    override def append(b: Sink): Unit = {
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
        b.indentInc.newline
        members.foreach(m => {
          m.append(b)
          b.newline
        })
        b.indentDec.newline
        b.append("}")
      }
    }
  }

  case class Param(name: String, typeName: String, modifier: Option[String] = None) extends ScalaCode {
    override def append(b: Sink): Unit = {
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

  case class BlockComment(comment: String) extends ScalaCode {
    override def append(b: Sink): Unit = {
      val lines = comment.split('\n')
      lines.headOption.map(line => {
        b.append("/**")
        b.append(line)
        b.newline
      })
      lines.tail.foreach(line => {
        b.append("  *")
        b.append(line)
        b.newline
      })
      b.append("  */")
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
