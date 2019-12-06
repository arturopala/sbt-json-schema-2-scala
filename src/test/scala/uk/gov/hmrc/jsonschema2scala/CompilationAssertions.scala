package uk.gov.hmrc.jsonschema2scala

import org.scalatest.Matchers

trait CompilationAssertions {
  self: CompilationAssertions with Matchers =>

  trait Assertion[T] {
    def assert(claim: T): Unit
  }

  def assertCompiles(sourceCodeUnits: Seq[Code], assertions: Assertion[ClassLoader]*): Unit =
    Compiler
      .compileCode(sourceCodeUnits, verbose = true)
      .fold(
        errors => fail(errors.mkString),
        cl => assertions.foreach(_.assert(cl))
      )

  case class ClassAssertion(classFullName: String, fieldNames: String*) extends Assertion[ClassLoader] {
    override def assert(classLoader: ClassLoader): Unit = {
      val clazz = classLoader.loadClass(classFullName)
      clazz should not be null
      //println(clazz.getDeclaredFields.map(_.getName).mkString(","))
      fieldNames.foreach { name =>
        clazz.getDeclaredField(name) should not be null
      }

    }
  }

}
