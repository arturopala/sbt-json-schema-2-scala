package uk.gov.hmrc.jsonschema2scala

import uk.gov.hmrc.jsonschema2scala.schema.Schema

import scala.annotation.tailrec

trait TypeResolver {

  def typeOf(
    schema: Schema,
    viewpoint: TypeDefinition,
    wrapAsOption: Boolean = true,
    showDefaultValue: Boolean = true): String

  def interfacesOf(schema: Schema, viewpoint: TypeDefinition): Set[String]

}

object TypeResolver {

  @tailrec
  final def shortenPrefix(path: List[String], otherPath: List[String], previous: Option[String] = None): List[String] =
    path match {
      case Nil => previous.map(_ :: path).getOrElse(path)
      case pathHead :: pathTail =>
        otherPath match {
          case Nil => previous.map(_ :: path).getOrElse(path)
          case otherHead :: otherTail =>
            if (pathHead != otherHead) previous.map(_ :: path).getOrElse(path)
            else shortenPrefix(pathTail, otherTail, Some(pathHead))
        }
    }

}
