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

import java.net.URLClassLoader
import java.nio.file.{Files, Path}

import scala.reflect.internal.util.{BatchSourceFile, Position}
import scala.reflect.io.AbstractFile
import scala.tools.nsc.reporters.AbstractReporter
import scala.tools.nsc.{Global, Settings}
import scala.util.Random

/**
  * Compiles scala code units for unit testing purposes
  */
object Compiler {

  private val tempDir: Path = Files.createTempDirectory("sbt-json-schema-2-scala-test")
  private val outputDir: Path = tempDir.resolve("classes")
  Files.createDirectory(outputDir)
  private val settings = createSettings(outputDir)

  def compileCode(code: Seq[Code], verbose: Boolean = false): Either[Seq[String], ClassLoader] = {
    val name = "Test_" + Random.alphanumeric.take(10)
    val content = Code.toString(code)
    if (verbose) println(content)
    compile(List((name, content)), verbose)
  }

  def compileSingle(sourceCode: String, verbose: Boolean = false): Either[Seq[String], ClassLoader] = {
    val name = "Test_" + Random.alphanumeric.take(10)
    compile(List((name, sourceCode)), verbose)
  }

  /**
    * Compile code units paired with the virtual file name
    */
  def compile(sourceCodeUnits: List[(String, String)], verbose: Boolean = false): Either[Seq[String], ClassLoader] = {

    val reporter = new CompilationReporter(settings)
    val global = new Global(settings, reporter)
    val run = new global.Run

    val sources = sourceCodeUnits.map {
      case (path, sourceCode) =>
        val file = Files.createTempFile(tempDir, path, ".scala")
        new BatchSourceFile(AbstractFile.getFile(file.toAbsolutePath.toString), sourceCode.toCharArray)
    }

    run.compileSources(sources)

    val errors: Seq[String] = reporter.errors.result
    if (errors.nonEmpty) {
      if (verbose) println(s"Compilation error(s) occurred [${errors.size}]:\n${errors.mkString("\n")}")
      Left(errors)
    } else {
      if (verbose) println("Compilation succeeded.")
      val classLoader: ClassLoader = new URLClassLoader(Array(outputDir.toUri.toURL), this.getClass.getClassLoader)
      Right(classLoader)
    }

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

  private def createSettings(outputDir: Path): Settings = {

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
    settings.outputDirs.setSingleOutput(AbstractFile.getDirectory(outputDir.toFile))
    settings
  }

}
