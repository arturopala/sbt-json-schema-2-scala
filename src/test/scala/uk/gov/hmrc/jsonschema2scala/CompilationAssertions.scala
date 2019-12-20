/*
 * Copyright 2019 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.jsonschema2scala

import java.nio.file.Paths

import org.scalatest.Matchers
import uk.gov.hmrc.jsonschema2scala.generator.Code

trait CompilationAssertions {
  self: Matchers =>

  trait Assertion[T] {
    def assert(claim: T): Unit
  }

  def assertSuccessAndCompiles(
    renderingResult: Either[List[String], (String, String, Seq[Code])],
    assertions: Assertion[ClassLoader]*)(implicit compiler: Compiler): Unit =
    renderingResult.fold[Unit](
      errors => fail(errors.mkString(", ")), {
        case (packageName: String, className: String, sourceCodeUnits: Seq[Code]) =>
          assertCompiles(
            packageName,
            className,
            sourceCodeUnits,
            assertions :+ ClassAssertion(s"$packageName.$className"): _*)
      }
    )

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
