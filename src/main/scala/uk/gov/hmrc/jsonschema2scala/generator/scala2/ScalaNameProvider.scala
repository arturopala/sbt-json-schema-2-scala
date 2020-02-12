package uk.gov.hmrc.jsonschema2scala.generator.scala2

import uk.gov.hmrc.jsonschema2scala.schema.{Schema, SchemaUtils}
import uk.gov.hmrc.jsonschema2scala.typer.NameProvider
import uk.gov.hmrc.jsonschema2scala.utils.{NameUtils, SHA256}

object ScalaNameProvider extends NameProvider {

  override def toTypeName(schema: Schema): String = {
    val name = SchemaUtils.referencedSchemaOption(schema).getOrElse(schema).name
    toIdentifier(NameUtils.normalizedFirstUppercase(name))
  }

  override def toFieldName(definition: Schema): String =
    toIdentifier(NameUtils.normalizedFirstLowercase(definition.name))

  override def toIdentifier(name: String, suffix: String = ""): String =
    maybeWrapInBackticks(name, suffix)

  final def maybeWrapInBackticks(name: String, suffix: String): String = {
    val n = if (objectMemberNames.contains(name + suffix)) "_" + name else name
    if (name.exists(hashEnablingCharacters.contains)) "$" + SHA256.hashOf(name, 3)
    else if (n.exists(c => !Character.isJavaIdentifierPart(c)) ||
             !Character.isJavaIdentifierStart(n.charAt(0)) ||
             scalaKeywords.contains(n)) {
      s"`${n.replaceAllLiterally("\\", "")}$suffix`"
    } else n + suffix
  }

  final def unwrapBackticks(name: String): String =
    if (name.length > 1 && name.startsWith("`") && name.endsWith("`")) name.substring(1, name.length - 1) else name

  def combine(prefix: String, suffix: String): String =
    maybeWrapInBackticks(unwrapBackticks(prefix) + unwrapBackticks(suffix), "")

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

  final val hashEnablingCharacters: Set[Char] = Set('^', '\\', '[', ']', '(', ')', '?', '+', '*')

}
