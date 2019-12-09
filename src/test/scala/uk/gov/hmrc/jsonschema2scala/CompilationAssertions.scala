package uk.gov.hmrc.jsonschema2scala

import java.nio.file.Paths

import org.scalatest.Matchers

trait CompilationAssertions {
  self: CompilationAssertions with Matchers =>

  trait Assertion[T] {
    def assert(claim: T): Unit
  }

  def assertCompiles(
    packageName: String,
    className: String,
    sourceCodeUnits: Seq[Code],
    assertions: Assertion[ClassLoader]*)(implicit compiler: Compiler): Unit = {
    val pathParts: Array[String] = (if (packageName.isEmpty) Array() else packageName.split(".")) ++ Array(
      className + ".scala")
    compiler
      .compileCode(Paths.get(pathParts.head, pathParts.tail: _*), sourceCodeUnits, verbose = true)
      .fold(
        errors => fail(s"Compilation of the source code has failed with ${errors.size} error(s)"),
        cl => assertions.foreach(_.assert(cl))
      )
  }

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
