package uk.gov.hmrc.jsonschema2scala.generator.scala2

import uk.gov.hmrc.jsonschema2scala.schema.NameUtils.{firstCharUppercase, normalize}
import uk.gov.hmrc.jsonschema2scala.schema.{ExternalSchemaReference, InternalSchemaReference, Schema}
import uk.gov.hmrc.jsonschema2scala.typer.{NameProvider, SHA256}

object ScalaNameProvider$ extends NameProvider {

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
