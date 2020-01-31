package uk.gov.hmrc.jsonschema2scala.schema

case class DebugOptions(
  enabled: Boolean = false,
  traceReadingProgress: Boolean = false,
  showJsonAfterAggregatingPartials: Boolean = false,
  showJsonAfterEmbeddingPropertiesIntoVariants: Boolean = false,
  showJsonAfterMergingWithAdditionalProperties: Boolean = false) {

  def show(header: String, content: String): Unit =
    if (enabled) {
      System.out.println(header)
      System.out.println(content)
    }

  private var indent = 0
  val indentWidth = 2

  def incIndent(s: String): String = {
    val i = indent
    indent = indent + 1
    s * i * indentWidth
  }

  def decIndent(s: String): String = {
    indent = indent - 1
    s * indent * indentWidth
  }

  def curIndent(s: String): String =
    s * indent * indentWidth
}
