package uk.gov.hmrc.jsonschema2scala.utils

import java.util.regex.Pattern

object NameUtils {

  final val NORMALIZE_PATTERN = Pattern.compile("""([_/@:$.-]+?\w)""")

  final def normalizedFirstUppercase(name: String): String =
    firstCharUppercase(normalize(removeSchemaFileSuffix(name)))

  final def normalizedFirstLowercase(name: String): String =
    firstCharLowercase(normalize(removeSchemaFileSuffix(name)))

  def removeSchemaFileSuffix(name: String): String =
    name
      .replace(".schema.json", "")
      .replace(".json", "")

  final def normalize(text: String): String = {
    val m = NORMALIZE_PATTERN.matcher(text)
    val sb = new StringBuilder
    var last = 0
    while (m.find) {
      sb.append(text.substring(last, m.start))
      sb.append(m.group(0).replaceAll("[_/@:$.-]", "").toUpperCase)
      last = m.end
    }
    sb.append(text.substring(last))
    sb.toString
  }

  final def firstCharUppercase(text: String): String =
    if (text.isEmpty) text
    else text.substring(0, 1).toUpperCase + text.substring(1)

  final def firstCharLowercase(text: String): String =
    if (text.isEmpty) text
    else text.substring(0, 1).toLowerCase + text.substring(1)

  final def singular(name: String): String =
    if (name.endsWith("Array")) name.substring(0, name.length - 5)
    else if (name.endsWith("List")) name.substring(0, name.length - 4)
    else name

  final def nextName(name: String): String = {
    val count = name.reverse.takeWhile(Character.isDigit).reverse
    name.dropRight(count.length) + (if (count.isEmpty) "_1" else (count.toInt + 1).toString)
  }

  @scala.annotation.tailrec
  final def nextDistinctName(name: String, existingNames: Set[String]): String =
    if (!existingNames.contains(name)) name else nextDistinctName(nextName(name), existingNames)

}
