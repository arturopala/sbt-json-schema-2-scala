package uk.gov.hmrc.jsonschema2scala.utils

object OptionOps {

  def getIfSingleItem[T](seqOpt: Option[Seq[T]]): Option[T] =
    seqOpt.flatMap(seq => if (seq.size == 1) Some(seq.head) else None)

  def someUnitIfEmpty[T](seqOpt: Option[Seq[T]]): Option[Unit] =
    seqOpt.flatMap(seq => if (seq.isEmpty) Some(()) else None)

  def emptyAsNone[T](seq: Seq[T]): Option[Seq[T]] = if (seq.isEmpty) None else Some(seq)

  def defined[T]: PartialFunction[Option[T], T] = {
    case Some(x) => x
  }
}
