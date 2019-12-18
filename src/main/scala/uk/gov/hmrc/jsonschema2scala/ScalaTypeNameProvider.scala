package uk.gov.hmrc.jsonschema2scala

import uk.gov.hmrc.jsonschema2scala.NameUtils.{firstCharUppercase, normalize}
import uk.gov.hmrc.jsonschema2scala.schema.{ExternalSchemaReference, InternalSchemaReference, Schema}

object ScalaTypeNameProvider extends TypeNameProvider {

  def toTypeName(schema: Schema): String = {
    val schemaName = schema match {
      case i: InternalSchemaReference =>
        i.schema.name
      case e: ExternalSchemaReference =>
        e.schema.name
      case s => s.name
    }
    firstCharUppercase(normalize(schemaName))
  }

  override def safe(name: String): String =
    if (name.exists(noNameChars.contains) || scalaKeywords.contains(name)) s"`$name`" else name

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

  final val noNameChars: Set[Char] = Set('@', '$', '-', '/', '.')
}
