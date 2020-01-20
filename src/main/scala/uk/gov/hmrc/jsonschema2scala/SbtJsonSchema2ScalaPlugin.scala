/*
 * Copyright 2020 HM Revenue & Customs
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

import java.io._

import sbt.Keys._
import sbt._
import uk.gov.hmrc.jsonschema2scala.generator.Code
import uk.gov.hmrc.jsonschema2scala.generator.scala2.{JsonSchema2ScalaFeature, ScalaCodeGenerator, ScalaCodeGeneratorOptions}
import uk.gov.hmrc.jsonschema2scala.schema.{Schema, SchemaReader, SchemaReferenceResolver, SchemaSourceFile}

import scala.io.Source

object SbtJsonSchema2ScalaPlugin extends AutoPlugin {

  override def requires = sbt.plugins.JvmPlugin

  override def trigger = allRequirements

  trait Keys {
    val jsonSchema2ScalaResources = settingKey[File]("Json schema sources directory")
    val jsonSchema2ScalaTargetDirectory = settingKey[File]("Generated scala code target directory")
    val jsonSchema2ScalaFeatures = settingKey[Set[JsonSchema2ScalaFeature]]("Scala code renderer options")
    val jsonSchema2ScalaPackageName = settingKey[String]("Scala code target package")
    val jsonSchema2ScalaGenerateScalaCodeTask = taskKey[Seq[File]]("Generate scala code from json schemas")
  }

  object Keys extends Keys

  object autoImport extends Keys {

    lazy val defaultJsonSchema2ScalaSettings: Seq[Def.Setting[_]] = Seq(
      jsonSchema2ScalaResources := baseDirectory.value / "src" / "main" / "schemas",
      jsonSchema2ScalaTargetDirectory := sourceManaged.value / "scala",
      jsonSchema2ScalaFeatures := JsonSchema2ScalaFeature.ALL,
      jsonSchema2ScalaPackageName := organization.value + ".models",
      sourceGenerators in Compile += jsonSchema2ScalaGenerateScalaCodeTask.taskValue,
      jsonSchema2ScalaGenerateScalaCodeTask := {
        JsonSchema2ScalaGenerateScalaCodeTask.apply(
          jsonSchema2ScalaResources.value,
          jsonSchema2ScalaTargetDirectory.value,
          ScalaCodeGeneratorOptions(
            features = jsonSchema2ScalaFeatures.value,
            packageName = jsonSchema2ScalaPackageName.value
          )
        )
      }
    )
  }

  import autoImport._

  override def projectSettings: Seq[Setting[_]] =
    inConfig(Compile)(defaultJsonSchema2ScalaSettings)

  object JsonSchema2ScalaGenerateScalaCodeTask {

    def apply(
      jsonSchemaSourceDirectory: File,
      scalaCodeTargetDirectory: File,
      options: ScalaCodeGeneratorOptions): Seq[File] =
      process(jsonSchemaSourceDirectory, scalaCodeTargetDirectory, options)

    def process(sourceDirectory: File, targetDirectory: File, options: ScalaCodeGeneratorOptions): Seq[File] = {
      if (!targetDirectory.exists()) targetDirectory.mkdirs()
      val generatedScalaFiles = listFiles(sourceDirectory, suffix = ".json") match {
        case files: Seq[File] if files.nonEmpty =>
          processJsonSchemas2Code(files, targetDirectory, options)
        case _ =>
          Seq()
      }
      val predefinedScalaUtilFiles =
        if (generatedScalaFiles.isEmpty) Seq()
        else {
          val variables = options2Variables(options)
          options.features.flatMap(filesSupportingFeature).map(copyResourceTo(targetDirectory, variables))
        }
      generatedScalaFiles ++ predefinedScalaUtilFiles
    }

    def processJsonSchemas2Code(
      jsonSchemaFiles: Seq[File],
      targetDirectory: File,
      options: ScalaCodeGeneratorOptions): Seq[File] = {

      println(s"Processing ${jsonSchemaFiles.size} schemas(s) with options $options")

      val schemaSources: Seq[SchemaSourceFile] = parseJsonSchemas(jsonSchemaFiles)

      val multiResolver: SchemaReferenceResolver = SchemaReferenceResolver(schemaSources)
      val schemas: Seq[(SchemaSourceFile, Schema)] =
        schemaSources
          .map(s => (s, SchemaReader.read(s, SchemaReferenceResolver(s, Some(multiResolver)))))

      schemas
        .map {
          case (schemaFile, definition) =>
            ScalaCodeGenerator
              .generateCodeFromSchema(
                definition,
                options,
                s"Generated from JSON Schema ${schemaFile.file.getName}"
              )
              .fold(
                errors => {
                  println(errors.zipWithIndex.map { case (e, i) => s"[$i] $e" }.mkString("\n"))
                  None
                }, {
                  case (packageName, className, code) => {
                    val content = Code.toString(code)
                    val directoryPath = packageName.split(".").mkString(File.separator)
                    val outputDirectory = new File(targetDirectory.getAbsolutePath + File.separator + directoryPath)
                    outputDirectory.mkdirs()
                    val outputFile = new File(outputDirectory, s"$className.scala")
                    println(s"Generating ${outputFile.toString} from ${schemaFile.file.getName}")
                    writeToFile(outputFile, content)
                    Some(outputFile)
                  }
                }
              )
        }
        .collect { case Some(x) => x }
    }

    def filesSupportingFeature(feature: JsonSchema2ScalaFeature): Seq[String] = feature match {
      case JsonSchema2ScalaFeature.Validator => Seq("Validator.scala.template")
      case JsonSchema2ScalaFeature.Generator => Seq("Generator.scala.template")
      case _                                 => Seq()
    }

    def listFiles(directory: File, suffix: String): Seq[File] =
      directory.listFiles(new FilenameFilter {
        override def accept(dir: File, name: String): Boolean = name.endsWith(suffix)
      })

    def parseJsonSchemas(files: Seq[File]): Seq[SchemaSourceFile] =
      files.map(SchemaSourceFile.apply)

    def options2Variables(options: ScalaCodeGeneratorOptions): Seq[(String, String)] =
      Seq("package" -> s"package ${options.packageName}")

    def copyResourceTo(targetDirectory: File, variables: Seq[(String, String)])(resourcePath: String): File = {
      val inputStream = getClass.getResourceAsStream(s"/$resourcePath")
      if (inputStream == null) throw new Exception(s"Resource $resourcePath not found on the classpath.")
      val content =
        Source.fromInputStream(inputStream, "utf-8").getLines().map(replaceVariables(variables)).mkString("\r\n")
      val targetFile = targetDirectory.toPath.resolve(resourcePath.replace(".template", "")).toFile
      writeToFile(targetFile, content)
      targetFile
    }

    def replaceVariables(variables: Seq[(String, String)])(content: String): String = variables.foldLeft(content) {
      case (c, (key, value)) => c.replace(s"/*$key*/", value)
    }

    def writeToFile(target: File, content: String): Unit = {
      target.createNewFile()
      val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(target), "utf-8"))
      try {
        writer.write(content)
        writer.flush()
      } finally {
        writer.close()
      }
    }
  }

}
