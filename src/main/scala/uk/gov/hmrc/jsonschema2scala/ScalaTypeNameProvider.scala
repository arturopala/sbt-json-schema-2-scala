package uk.gov.hmrc.jsonschema2scala

import uk.gov.hmrc.jsonschema2scala.Names.{firstCharUppercase, normalize}
import uk.gov.hmrc.jsonschema2scala.schema.{ExternalReference, InternalReference, Schema}

object ScalaTypeNameProvider extends TypeNameProvider {

  def toTypeName(schema: Schema): String = {
    val schemaName = schema match {
      case i: InternalReference =>
        i.schema.name
      case e: ExternalReference =>
        e.schema.name
      case s => s.name
    }
    firstCharUppercase(normalize(schemaName))
  }

  def safe(name: String): String =
    if (name.exists(noNameChars.contains) || scalaKeywords.contains(name)) s"`$name`" else name

  val scalaKeywords: Set[String] = Set(
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

  val noNameChars: Set[Char] = Set('@', '$', '-', '/', '.')
}
