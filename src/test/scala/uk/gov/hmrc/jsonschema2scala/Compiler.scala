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

import java.nio.file.{Files, Path}

import scala.reflect.internal.util.{BatchSourceFile, Position}
import scala.reflect.io.AbstractFile
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.AbstractReporter
import scala.util.Random

/**
  * Compiles scala code units for unit testing purposes
  */
object Compiler {

  private val tempDir: Path = Files.createTempDirectory("sbt-json-schema-2-scala-test")
  private val settings = createSettings()

  def compileCode(code: Seq[Code], show: Boolean = false): Unit = {
    val name = "Test_" + Random.alphanumeric.take(10)
    val content = Code.toString(code)
    if (show) println(content)
    compile(List((name, content)))
  }

  def compileSingle(code: String): Unit = {
    val name = "Test_" + Random.alphanumeric.take(10)
    compile(List((name, code)))
  }

  /**
    * Compile code units paired with the virtual file name
    */
  def compile(codeUnits: List[(String, String)]): Unit = {

    val reporter = new CompilationReporter(settings)
    val global = new Global(settings, reporter)
    val run = new global.Run

    val sources = codeUnits.map {
      case (path, content) =>
        val file = Files.createTempFile(tempDir, path, ".scala")
        new BatchSourceFile(AbstractFile.getFile(file.toAbsolutePath.toString), content.toCharArray)
    }

    run.compileSources(sources)

    val errors = reporter.errors.result
    if (errors.nonEmpty) throw new CompilationError(s"${errors.size} error(s) occurred:\n${errors.mkString("\n")}")
  }

  private class CompilationReporter(val settings: Settings) extends AbstractReporter {
    val errors = List.newBuilder[String]

    def display(pos: Position, msg: String, severity: Severity) {
      val m = Position.formatMessage(pos, msg, true)
      if (severity == ERROR)
        errors += m
      else println(m)
    }

    def displayPrompt() {}
  }

  class CompilationError(msg: String) extends RuntimeException(msg)

  private def createSettings(): Settings = {
    val settings = new Settings(s => {
      sys.error("errors report: " + s)
    })

    val classLoader = this.getClass.getClassLoader
    val urls = classLoader match {
      case urlClassLoader: java.net.URLClassLoader => urlClassLoader.getURLs.toList
      case a                                       => sys.error("Was expecting an URLClassLoader, found a " + a.getClass)
    }
    val classpath = urls map { _.toString }
    settings.bootclasspath.value = classpath.distinct.mkString(java.io.File.pathSeparator)
    settings.classpath.value = classpath.distinct.mkString(java.io.File.pathSeparator)
    settings.embeddedDefaults(classLoader)
    settings
  }

}
