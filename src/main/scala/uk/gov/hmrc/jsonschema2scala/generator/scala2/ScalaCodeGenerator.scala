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

package uk.gov.hmrc.jsonschema2scala.generator.scala2

import uk.gov.hmrc.jsonschema2scala.generator.scala2.ScalaCode._
import uk.gov.hmrc.jsonschema2scala.generator.{CodeGenerator, ScalaTypeResolver, TypeResolver}
import uk.gov.hmrc.jsonschema2scala.schema.NameUtils.{firstCharUppercase, normalize, pathLastPart, variableName}
import uk.gov.hmrc.jsonschema2scala.schema._
import uk.gov.hmrc.jsonschema2scala.typer.{ScalaTypeNameProvider, TypeDefinition, TypeDefinitionsBuilder, TypeNameProvider}

object ScalaCodeGenerator extends CodeGenerator with KnownFieldGenerators {

  val maxNumberOfArgs = 22

  override type CodeGeneratorOptions = ScalaCodeGeneratorOptions

  override def generateCodeFrom(
    schema: Schema,
    options: ScalaCodeGeneratorOptions,
    description: String): CodeGeneratorResult = {

    val typeNameProvider: TypeNameProvider = ScalaTypeNameProvider

    TypeDefinitionsBuilder
      .buildFrom(schema)(typeNameProvider)
      .fold(
        errors => Left(errors),
        typeDef => {

          val schemaUrlToTypePath: Map[String, List[String]] = TypeDefinition.listSchemaUriToTypePath(typeDef).toMap
          val schemaUrlToTypeInterfaces: Map[String, Seq[List[String]]] =
            TypeDefinition.listSchemaUriToTypeInterfaces(typeDef).groupBy(_._1).mapValues(_.flatMap(_._2))

          val typeResolver: TypeResolver =
            new ScalaTypeResolver(schemaUrlToTypePath, schemaUrlToTypeInterfaces)(typeNameProvider)

          generateCodeFrom(typeDef, options, ScalaCodeGeneratorContext(schema, options), description)(
            typeResolver,
            typeNameProvider)
        }
      )
  }

  def generateCodeFrom(
    typeDef: TypeDefinition,
    options: ScalaCodeGeneratorOptions,
    context: ScalaCodeGeneratorContext,
    description: String)(
    implicit typeResolver: TypeResolver,
    typeNameProvider: TypeNameProvider): CodeGeneratorResult = {

    val code: Seq[Option[ScalaCode]] =
      Seq(
        Some(BlockComment(
          s"""
             | ---------------------------------------------------
             | THIS FILE HAS BEEN GENERATED - DO NOT MODIFY IT !!!
             |          CHANGE THE JSON SCHEMA IF NEEDED
             | ---------------------------------------------------
             | $description
             | ${renderTypesOverview(typeDef)}
             |""".stripMargin,
          doc = false
        ))) ++ Seq(Some(Package(options.packageName))) ++
        generateGlobalImports(context) ++
        generateTypeDefinition(typeDef, isTopLevel = true, context)

    Right((options.packageName, typeDef.name, code.collect(defined)))
  }

  def generateGlobalImports(context: ScalaCodeGeneratorContext): Seq[Option[ScalaCode]] =
    Seq(
      context.generatorsOpt.map(_ => Import("org.scalacheck", List("Arbitrary", "Gen"))),
      context.playJsonOpt.map(_ => WildcardImport("play.api.libs.json")))

  def renderTypesOverview(typeDef: TypeDefinition): String = {
    val types = computeTypesTree(typeDef, 0)
    if (types.size <= 1) ""
    else "Content:\n\n" + types.map { case (name, level) => "    " + (".  " * level) + name }.mkString("\n") + "\n\n"
  }

  def computeTypesTree(typeDef: TypeDefinition, level: Int): Seq[(String, Int)] =
    Seq((typeDef.name, level)) ++ typeDef.nestedTypes
      .filterNot(_.forReferenceOnly)
      .flatMap(computeTypesTree(_, level + 1))

  def generateTypeDefinition(typeDef: TypeDefinition, isTopLevel: Boolean, context: ScalaCodeGeneratorContext)(
    implicit typeResolver: TypeResolver,
    typeNameProvider: TypeNameProvider): Seq[Option[ScalaCode]] = {

    lazy val classFields: Seq[Param] =
      generateClassFields(typeDef)

    lazy val propertyValidators: Seq[Option[ScalaCode]] =
      generatePropertyValidators(typeDef, context)

    lazy val objectValidator: String =
      generateObjectValidator(typeDef, context)

    lazy val fieldGenerators: String =
      generateFieldGenerators(typeDef, context)

    lazy val fieldsInitialization: String =
      generateGenFieldsInitialization(typeDef)

    lazy val sanitizers: Seq[Option[ScalaCode]] =
      generateSanitizers(typeDef, context)

    lazy val sanitizerList: String =
      generateSanitizerList(typeDef)

    lazy val commonObject: Seq[Option[ScalaCode]] =
      if (isTopLevel) generateCommonObjectDeclaration(context) else Seq.empty

    lazy val nestedTypesDefinitions: Seq[Option[ScalaCode]] = typeDef.nestedTypes
      .filterNot(_.forReferenceOnly)
      .flatMap(t => generateTypeDefinition(t, isTopLevel = false, context))

    lazy val objectMembersCode: Seq[Option[ScalaCode]] = if (isTopLevel) {
      generateObjectMembers(typeDef, context)
    } else Seq.empty

    lazy val generatorsCode: Seq[Option[ScalaCode]] =
      generateGenerators(typeDef, fieldGenerators, fieldsInitialization, context)

    lazy val validatorCode: Seq[Option[ScalaCode]] =
      generateValidator(typeDef, objectValidator, context)

    lazy val sanitizersCode: Seq[Option[ScalaCode]] =
      generateSanitizers(typeDef, sanitizers, sanitizerList, context)

    lazy val jsonFormatsCode: Seq[Option[ScalaCode]] =
      generateJsonFormats(typeDef, context)

    // -----------------------------------------
    //    CASE CLASS AND OBJECT TEMPLATE
    // -----------------------------------------

    /*val classCode2 =
      if (typeDef.isInterface)
        s"""sealed trait ${typeDef.name} {${generateInterfaceMethods(typeDef)}}""".stripMargin
      else
        s"""case class ${typeDef.name}(
        |  $classFields${if (isTopLevel && context.renderGenerators)
          s""",
             |  id: Option[String] = None
             |) extends Record${if (typeDef.hasInterfaces) " with " else ""}${generateClassInterfacesSignature(typeDef)} {
             |
             |  override def uniqueKey: Option[String] = ${context.uniqueKey
               .map(key => s"${key._1}.map(${typeDef.name}.uniqueKey)")
               .getOrElse("None")}
             |  override def lookupKeys: Seq[String] = Seq(${context.keys
               .map(key => s"${key._1}.map(${typeDef.name}.${key._2}Key)")
               .mkString(", ")})${if (context.keys.nonEmpty) ".collect{case Some(x) => x}" else ""}
             |  override def withId(id: Option[String]): ${typeDef.name} = copy(id = id)
             |""".stripMargin
        else
          s")${if (typeDef.hasInterfaces) " extends " else ""}${generateClassInterfacesSignature(typeDef)} {"}
        |
        |  ${if (context.renderBuilders) generateBuilderMethods(typeDef) else ""}
        |}"""*/

    val classCode: Option[ScalaCode] =
      if (typeDef.isInterface)
        Some(
          Trait(
            name = typeDef.name,
            members = generateInterfaceMethods(findCommonFields(typeDef.subtypes, typeDef)),
            modifier = Some("sealed"),
            comment =
              Some(s"${typeDef.schema.description.map(d => s"$d\n").getOrElse("")}Schema: ${typeDef.schema.uri}")
          ))
      else {
        val parameters = classFields ++ (if (isTopLevel && context.renderGenerators)
                                           Seq(Param("id", "Option[String] = None"))
                                         else Seq.empty)
        if (parameters.nonEmpty) {
          Some(
            CaseClass(
              name = typeDef.name,
              parameters = parameters,
              supertypes = (if (isTopLevel && context.renderGenerators) Seq("Record") else Seq.empty) ++ compileClassInterfaceList(
                typeDef),
              members = if (context.renderBuilders) generateBuilderMethods(typeDef) else Seq.empty,
              comment =
                Some(s"${typeDef.schema.description.map(d => s"$d\n").getOrElse("")}Schema: ${typeDef.schema.uri}")
            ))
        } else if (!isTopLevel)
          Some(
            Trait(
              name = typeDef.name,
              comment =
                Some(s"${typeDef.schema.description.map(d => s"$d\n").getOrElse("")}Schema: ${typeDef.schema.uri}")
            ))
        else None
      }

    val objectCode: Option[ScalaCode] =
      Object(
        name = typeDef.name,
        supertypes = if (context.renderGenerators) Seq(s"RecordUtils[${typeDef.name}]") else Seq.empty,
        members = Seq(
          objectMembersCode,
          propertyValidators,
          validatorCode,
          generatorsCode,
          sanitizersCode,
          jsonFormatsCode,
          nestedTypesDefinitions,
          commonObject).flatten.collect(defined)
      ).asOption

    Seq(classCode, objectCode)
  }

  def generateObjectMembers(typeDef: TypeDefinition, context: ScalaCodeGeneratorContext): Seq[Option[ScalaCode]] =
    Seq(
      context.validatorsOpt.map(_ => WildcardImport("Validator")),
      context.generatorsOpt.map(_ => WildcardImport("Generator.GenOps")),
      context.generatorsOpt.map(_ =>
        ValueDefinition("arbitrary", "Arbitrary[Char]", Seq("Arbitrary(Gen.alphaNumChar)"), Some("implicit"))),
      context.generatorsOpt.map(
        _ =>
          ValueDefinition(
            name = "recordType",
            returnType = s"RecordMetaData[${typeDef.name}]",
            body = Seq(s"RecordMetaData[${typeDef.name}](this)"),
            modifier = Some("implicit"))),
      context.generatorsOpt.flatMap(
        _ =>
          context.uniqueKey.map(
            key =>
              MethodDefinition(
                name = "uniqueKey",
                parameters = Seq(Param("key", "String")),
                returnType = "String",
                body = Seq(quoted(s"${key._2}:$${key.toUpperCase}")))))
    ) ++ (if (context.renderGenerators)
            context.keys.map(
              key =>
                Some(
                  MethodDefinition(
                    name = s"${key._2}Key",
                    parameters = Seq(Param("key", "String")),
                    returnType = "String",
                    body = Seq(quoted(s"${key._2}:$${key.toUpperCase}")))))
          else Seq.empty)

  def compileClassInterfaceList(typeDef: TypeDefinition)(implicit typeResolver: TypeResolver): List[String] =
    (typeDef.interfaces
      .map(it => typeResolver.typeOf(it.schema, typeDef, wrapAsOption = false, showDefaultValue = false))
      .distinct
      .toList ++ typeResolver.interfacesOf(typeDef.schema, typeDef)).sorted

  def generateClassFields(
    typeDef: TypeDefinition)(implicit typeResolver: TypeResolver, typeNameProvider: TypeNameProvider): Seq[Param] =
    typeDef.schema.properties
      .take(maxNumberOfArgs)
      .sortBy(fieldOrder)
      .map(
        schema =>
          Param(
            name = typeNameProvider.toIdentifier(schema.name),
            typeName = typeResolver.typeOf(schema, typeDef),
            comment = schema.description
        ))

  def fieldOrder(schema: Schema): Int = if (schema.required) 0 else if (schema.boolean) 1 else 2

  def generateInterfaceMethods(interfaceFields: Set[(String, String)])(
    implicit typeNameProvider: TypeNameProvider): Seq[ScalaCode] =
    interfaceFields.toSeq
      .sortBy(_._1)
      .map {
        case (fieldName, fieldTypeName) =>
          MethodDefinition(
            name = typeNameProvider.toIdentifier(fieldName),
            parameters = Seq.empty,
            returnType = fieldTypeName,
            body = Seq.empty,
            modifier = None)
      }

  def findCommonFields(types: Seq[TypeDefinition], viewpoint: TypeDefinition)(
    implicit typeResolver: TypeResolver): Set[(String, String)] =
    types
      .map(_.schema)
      .map {
        case o: ObjectSchema =>
          o.properties
            .map(schema => (schema.name, typeResolver.typeOf(schema, viewpoint, showDefaultValue = false)))
            .toSet
        case _ => Set.empty[(String, String)]
      } match {
      case s if s.isEmpty => Set.empty
      case s              => s.reduce[Set[(String, String)]]((a, b) => a.intersect(b))
    }

  def generateFieldGenerators(typeDef: TypeDefinition, context: ScalaCodeGeneratorContext)(
    implicit typeResolver: TypeResolver,
    typeNameProvider: TypeNameProvider): String =
    typeDef.schema.properties
      .filter(_.required)
      .take(maxNumberOfArgs)
      .map(prop => s"""${variableName(prop)} <- ${generateValueGenerator(typeDef, prop, context)}""".stripMargin)
      .mkString("\n    ")

  def generateGenFieldsInitialization(typeDef: TypeDefinition)(implicit typeNameProvider: TypeNameProvider): String =
    typeDef.schema.properties
      .filter(_.required)
      .take(maxNumberOfArgs)
      .map(prop => s"""${typeNameProvider.toIdentifier(prop.name)} = ${variableName(prop)}""".stripMargin)
      .mkString("\n    ", ",\n    ", "\n  ")

  def generateBuilderMethods(
    typeDef: TypeDefinition)(implicit typeResolver: TypeResolver, typeNameProvider: TypeNameProvider): Seq[ScalaCode] =
    typeDef.schema.properties
      .take(maxNumberOfArgs)
      .flatMap(schema => {
        val typeName = typeResolver.typeOf(schema, typeDef, showDefaultValue = false)
        Seq(
          MethodDefinition(
            name = s"with${firstCharUppercase(normalize(schema.name))}",
            parameters = Seq(Param(typeNameProvider.toIdentifier(schema.name), typeName)),
            returnType = typeDef.name,
            body = Seq(
              s"copy(${typeNameProvider.toIdentifier(schema.name)} = ${typeNameProvider.toIdentifier(schema.name)})")
          ),
          MethodDefinition(
            name = s"modify${firstCharUppercase(normalize(schema.name))}",
            parameters = Seq(Param("pf", s"PartialFunction[$typeName, $typeName]")),
            returnType = typeDef.name,
            body = Seq(s"if (pf.isDefinedAt(${typeNameProvider.toIdentifier(schema.name)})) copy(${typeNameProvider
              .toIdentifier(schema.name)} = pf(${typeNameProvider.toIdentifier(schema.name)})) else this")
          )
        )
      })

  def generateValueGenerator(
    hostType: TypeDefinition,
    property: Schema,
    context: ScalaCodeGeneratorContext,
    wrapOption: Boolean = true)(implicit typeResolver: TypeResolver, typeNameProvider: TypeNameProvider): String = {
    val gen = knownFieldGenerators(property.name)
      .orElse(knownFieldGenerators(pathLastPart(property)))
      .getOrElse(property match {
        case s: StringSchema =>
          s.custom[String]("x_gen")
            .getOrElse(if (s.enum.isDefined) {
              if (s.enum.get.size == 1)
                s"""Gen.const("${s.enum.get.head}")"""
              else
                s"""Gen.oneOf(${context.commonReference(s"Seq(${s.enum.get.mkString("\"", "\",\"", "\"")})")})"""
            } else if (s.pattern.isDefined)
              s"""Generator.regex(${context.commonReference(quoted(s.pattern.get))})"""
            else if (s.minLength.isDefined || s.maxLength.isDefined)
              s"""Generator.stringMinMaxN(${s.minLength.getOrElse(1)},${s.maxLength.getOrElse(256)})"""
            else "Generator.stringMaxN(256)")

        case n: NumberSchema =>
          n.custom[String]("x_gen")
            .getOrElse((n.minimum, n.maximum, n.multipleOf) match {
              case (Some(min), Some(max), mlt) => s"Generator.chooseBigDecimal($min,$max,$mlt)"
              case (Some(min), None, mlt)      => s"Generator.chooseBigDecimal($min,100000000,$mlt)"
              case (None, Some(max), mlt)      => s"Generator.chooseBigDecimal(0,$max,$mlt)"
              case _                           => "Gen.const(BigDecimal(0))"
            })

        case n: IntegerSchema =>
          n.custom[String]("x_gen")
            .getOrElse((n.minimum, n.maximum, n.multipleOf) match {
              case (Some(min), Some(max), mlt) => s"Generator.chooseNum($min,$max,$mlt)"
              case (Some(min), None, mlt)      => s"Generator.chooseNum($min,100000000,$mlt)"
              case (None, Some(max), mlt)      => s"Generator.chooseNum(0,$max,$mlt)"
              case _                           => "Gen.const(Int(0))"
            })

        case b: BooleanSchema => "Generator.booleanGen"
        case a: ArraySchema =>
          s"Generator.nonEmptyListOfMaxN(1,${a.items
            .map {
              case item :: Nil => generateValueGenerator(hostType, item, context, false)
              case _           => "???" //FIXME add multi-item array support
            }
            .getOrElse("Gen.chooseNum(1, 1000)")})"
        case o: ObjectSchema => s"${typeNameProvider.toTypeName(o)}.gen"

        case o: OneOfAnyOfSchema =>
          if (o.variants.isEmpty) "???"
          else if (o.variants.size == 1) generateValueGenerator(hostType, o.variants.head, context)
          else
            o.variants.head match {
              case _: ObjectSchema => s"${typeNameProvider.toTypeName(o)}.gen"
              case _ =>
                s"Gen.oneOf[${typeNameProvider.toTypeName(o)}](${o.variants
                  .map(v => s"${generateValueGenerator(hostType, v, context)}.map(_.asInstanceOf[${typeResolver.typeOf(v, hostType, showDefaultValue = false)}])")
                  .mkString(",\n  ")})"
            }
        case e: ExternalSchemaReference => s"${typeNameProvider.toTypeName(e)}.gen"
      })

    val genWithConstraints = property match {
      case s: StringSchema =>
        val withMinLength = s.minLength.map(minLength => s"""$gen.suchThat(_.length>=$minLength)""").getOrElse(gen)
        val withMaxLength =
          s.maxLength.map(maxLength => s"""$withMinLength.suchThat(_.length<=$maxLength)""").getOrElse(withMinLength)
        withMaxLength
      case _ => gen
    }

    if (!property.required && wrapOption) s"""Generator.optionGen($genWithConstraints)""" else genWithConstraints
  }

  def generatePropertyValidators(typeDef: TypeDefinition, context: ScalaCodeGeneratorContext)(
    implicit typeResolver: TypeResolver,
    typeNameProvider: TypeNameProvider): Seq[Option[ScalaCode]] =
    if (!context.renderValidators) Seq.empty
    else
      typeDef.schema.properties
        .take(maxNumberOfArgs)
        .map(prop => generateValueValidator(prop, context, extractProperty = false).map((prop, _)))
        .collect {
          case Some((prop, validator)) =>
            Some(
              ValueDefinition(
                name = s"${prop.name}Validator",
                returnType = s"Validator[${typeResolver.typeOf(prop, typeDef, showDefaultValue = false)}]",
                body = Seq(validator),
                modifier = None
              ))
        }

  def generateObjectValidator(typeDef: TypeDefinition, context: ScalaCodeGeneratorContext)(
    implicit typeNameProvider: TypeNameProvider): String = {
    val propertyValidatorsCalls = typeDef.schema.properties
      .take(maxNumberOfArgs)
      .map(prop => generateValueValidatorCall(prop, context))
      .collect { case Some(validator) => s"""$validator""".stripMargin }
    val validators =
      if (typeDef.schema.alternativeRequiredFields.isEmpty) propertyValidatorsCalls
      else
        propertyValidatorsCalls :+
          s"""  checkIfOnlyOneSetIsDefined(${typeDef.schema.alternativeRequiredFields
            .map(_.map(a => {
              typeDef.schema.properties
                .find(_.name == a)
                .map(prop => s"_.$a${if (prop.boolean) ".asOption" else ""}")
                .get
            }).mkString("Set(", ",", ")"))
            .mkString("Seq(", ",", ")")},"${typeDef.schema.alternativeRequiredFields
            .map(_.mkString("{", ",", "}"))
            .mkString("[", ",", "]")}")"""
    validators.mkString(",\n  ")
  }

  def generateValueValidator(
    property: Schema,
    context: ScalaCodeGeneratorContext,
    isMandatory: Boolean = false,
    extractProperty: Boolean = true)(
    implicit typeResolver: TypeResolver,
    typeNameProvider: TypeNameProvider): Option[String] = {
    val propertyReference = if (extractProperty) s"_.${property.name}" else "_"
    val propertyExtractor = if (extractProperty) s"_.${property.name}" else "identity"
    property match {
      case s: StringSchema =>
        if (s.enum.isDefined) Some(s"""  check($propertyReference.isOneOf(${context.commonReference(s"Seq(${s.enum.get
          .mkString("\"", "\",\"", "\"")})")}), "Invalid ${property.name}, does not match allowed values")""")
        else if (s.pattern.isDefined)
          Some(s"""  check($propertyReference.matches(${context
            .commonReference(context.commonReference(quoted(s.pattern.get)))}), s${quoted(
            s"Invalid ${property.name}, does not matches regex $${${context.commonReference(quoted(s.pattern.get))}}")})""")
        else if (s.minLength.isDefined && s.maxLength.isDefined)
          Some(
            s"""  check($propertyReference.lengthMinMaxInclusive(${s.minLength.get},${s.maxLength.get}), "Invalid length of ${property.name}, should be between ${s.minLength.get} and ${s.maxLength.get} inclusive")""")
        else if (s.minLength.isDefined)
          Some(
            s"""  check($propertyReference.lengthMin(${s.minLength.get}),"Invalid length of ${property.name}, minimum length should be ${s.minLength.get}")""")
        else if (s.maxLength.isDefined)
          Some(
            s"""  check($propertyReference.lengthMax(${s.maxLength.get}),"Invalid length of ${property.name}, maximum length should be ${s.maxLength.get}")""")
        else None

      case n: NumberSchema =>
        (n.minimum, n.maximum, n.multipleOf) match {
          case (Some(min), Some(max), mlt) =>
            Some(s"""  check($propertyReference.inRange(BigDecimal($min),BigDecimal($max),${mlt.map(a =>
              s"BigDecimal($a)")}),"Invalid number ${property.name}, must be in range <$min,$max>")""")
          case (None, Some(max), mlt) =>
            Some(s"""  check($propertyReference.lteq(BigDecimal($max),${mlt
              .map(a => s"BigDecimal($a)")}),"Invalid number ${property.name}, must be lower than or equal to $max")""")
          case (Some(min), None, mlt) =>
            Some(s"""  check($propertyReference.gteq(BigDecimal($min),${mlt
              .map(a => s"BigDecimal($a)")}),"Invalid number ${property.name}, must be greater than or equal to $min")""")
          case _ => None
        }

      case a: ArraySchema =>
        val itemValidator: Option[String] = (a.items match {
          case Some((o: ObjectSchema) :: Nil) => Some(s"""${typeNameProvider.toTypeName(o)}.validate""")
          case Some(x :: Nil)                 => generateValueValidator(x, context)
          case _                              => None //FIXME add multi-item array support
        }).map(vv =>
          if (property.required || isMandatory) s""" checkEach($propertyExtractor, $vv)"""
          else s"""  checkEachIfSome($propertyExtractor, $vv)""")
        val minValidatorOpt = a.minItems.map(min =>
          s"""   check(_.size >= $min,"Invalid array size, must be greater than or equal to $min")""")
        val maxValidatorOpt = a.maxItems.map(max =>
          s"""   check(_.size <= $max,"Invalid array size, must be lower than or equal to $max")""")
        if (itemValidator.isDefined && (minValidatorOpt.isDefined || maxValidatorOpt.isDefined))
          Some(s"""   Validator(${Seq(itemValidator, minValidatorOpt, maxValidatorOpt)
            .collect { case Some(x) => x }
            .mkString(",")})""")
        else itemValidator

      case _: ObjectSchema =>
        if (property.required || isMandatory)
          Some(s""" checkProperty($propertyExtractor, ${typeNameProvider.toTypeName(property)}.validate)""")
        else Some(s"""  checkIfSome($propertyExtractor, ${typeNameProvider.toTypeName(property)}.validate)""")

      case o: OneOfAnyOfSchema =>
        if (o.variants.isEmpty) None
        else if (o.variants.size == 1) generateValueValidator(o.variants.head, context, o.required)
        else
          o.variants.head match {
            case _: ObjectSchema =>
              if (property.required || isMandatory)
                Some(s""" checkProperty($propertyExtractor, ${typeNameProvider.toTypeName(property)}.validate)""")
              else Some(s"""  checkIfSome($propertyExtractor, ${typeNameProvider.toTypeName(property)}.validate)""")
            case _ =>
              generateValueValidator(o.variants.head, context, o.required)
          }

      case _: BooleanSchema => None
      case _: ExternalSchemaReference =>
        if (property.required || isMandatory)
          Some(s""" checkProperty($propertyExtractor, ${typeNameProvider.toTypeName(property)}.validate)""")
        else Some(s"""  checkIfSome($propertyExtractor, ${typeNameProvider.toTypeName(property)}.validate)""")
    }
  }

  def generateValueValidatorCall(property: Schema, context: ScalaCodeGeneratorContext, isMandatory: Boolean = false)(
    implicit typeNameProvider: TypeNameProvider): Option[String] =
    property match {
      case d: Schema if d.validated =>
        Some(s"""  checkProperty(_.${typeNameProvider.toIdentifier(property.name)}, ${property.name}Validator)""")
      case _ => None
    }

  def generateSanitizerList(typeDef: TypeDefinition): String = {
    val simpleSanitizerList = typeDef.schema.properties
      .filter(p => !(p.required && p.primitive) && !typeDef.schema.alternativeRequiredFields.exists(_.contains(p.name)))
      .take(maxNumberOfArgs)
      .map(prop => s"${prop.name}Sanitizer")
    val sanitizerList =
      if (typeDef.schema.alternativeRequiredFields.isEmpty) simpleSanitizerList
      else
        simpleSanitizerList :+ s"${generateComposedFieldName(typeDef.schema.alternativeRequiredFields.map(_.head), "Or")}AlternativeSanitizer"
    sanitizerList.mkString(",\n  ")
  }

  def generateComposedFieldName(parts: Seq[String], sep: String): String =
    (parts.head +: parts.tail.map(p => p.take(1).toUpperCase + p.drop(1))).mkString(sep)

  def generateSanitizers(typeDef: TypeDefinition, context: ScalaCodeGeneratorContext)(
    implicit typeResolver: TypeResolver,
    typeNameProvider: TypeNameProvider): Seq[Option[ScalaCode]] = {
    val simpleSanitizers: Seq[Option[ScalaCode]] = typeDef.schema.properties
      .take(maxNumberOfArgs)
      .toList
      .map(schema =>
        if (schema.required) {
          if (schema.primitive) None
          else
            Some(
              ValueDefinition(
                name = s"${schema.name}Sanitizer",
                returnType = "Update",
                body = Seq(s"""seed => entity =>
                              |    entity.copy(${typeNameProvider.toIdentifier(schema.name)} = ${schema match {
                                case o: ObjectSchema =>
                                  s"${typeNameProvider.toTypeName(o)}.sanitize(seed)(entity.${typeNameProvider
                                    .toIdentifier(schema.name)})"
                                case a: ArraySchema if !a.allItemsPrimitive =>
                                  s"entity.${typeNameProvider.toIdentifier(schema.name)}.map(item => ${typeNameProvider
                                    .toTypeName(a.items.get.head)}.sanitize(seed)(item))"
                                case o: OneOfAnyOfSchema if o.variants.nonEmpty && !o.variants.head.primitive =>
                                  if (o.variants.size == 1)
                                    s"${typeNameProvider.toTypeName(o.variants.head)}.sanitize(seed)(entity.${typeNameProvider
                                      .toIdentifier(schema.name)})"
                                  else
                                    o.variants
                                      .map(v =>
                                        s"case x:${typeNameProvider.toTypeName(v)} => ${typeNameProvider.toTypeName(v)}.sanitize(seed)(x)")
                                      .mkString(
                                        s"entity.${typeNameProvider.toIdentifier(schema.name)} match {\n  ",
                                        "\n  ",
                                        "\n}")
                                case _ => s"entity.${typeNameProvider.toIdentifier(schema.name)}"
                              }})
         """.stripMargin)
              ))
        } else
          Some(ValueDefinition(
            name = s"${schema.name}Sanitizer",
            returnType = "Update",
            body = Seq(if (schema.primitive) {
              if (schema.validated)
                s"""seed => entity =>
                   |    entity.copy(${schema.name} = ${schema.name}Validator(entity.${schema.name}).fold(_ => None, _ => entity.${schema.name})
                   |      .orElse(Generator.get(${generateValueGenerator(typeDef, schema, context, wrapOption = false)})(seed)))
                   """.stripMargin
              else
                s"""seed => entity =>
                   |    entity.copy(${schema.name} = Generator.get(${generateValueGenerator(
                     typeDef,
                     schema,
                     context,
                     wrapOption = false)})(seed))
           """.stripMargin
            } else
              s"""seed => entity =>
                 |    entity.copy(${typeNameProvider.toIdentifier(schema.name)} = entity.${typeNameProvider
                   .toIdentifier(schema.name)}
                 |      .orElse(Generator.get(${generateValueGenerator(typeDef, schema, context, wrapOption = false)})(seed))${generateSanitizerSuffix(
                   schema)})
           """.stripMargin)
          )))

    if (typeDef.schema.alternativeRequiredFields.isEmpty) simpleSanitizers
    else simpleSanitizers ++ generateAlternativeSanitizers(typeDef, context)
  }

  def generateAlternativeSanitizers(typeDef: TypeDefinition, context: ScalaCodeGeneratorContext)(
    implicit typeResolver: TypeResolver,
    typeNameProvider: TypeNameProvider): Seq[Option[ScalaCode]] = {

    val compoundSanitizers: Seq[Option[ScalaCode]] = typeDef.schema.alternativeRequiredFields.toList
      .map(
        set =>
          generateCompoundSanitizer(
            typeDef,
            set,
            typeDef.schema.alternativeRequiredFields.filterNot(_ == set).reduce(_ ++ _),
            context))

    compoundSanitizers :+ Some(
      ValueDefinition(
        name =
          s"${generateComposedFieldName(typeDef.schema.alternativeRequiredFields.map(_.head), "Or")}AlternativeSanitizer",
        returnType = "Update",
        body = Seq(
          s"""seed => entity =>
             |          ${typeDef.schema.alternativeRequiredFields
               .map(set =>
                 s"if(entity.${set.head}.isDefined) ${generateComposedFieldName(set.toSeq, "And")}CompoundSanitizer(seed)(entity)")
               .mkString("\nelse      ")}
             |          else Generator.get(Gen.chooseNum(0,${typeDef.schema.alternativeRequiredFields.size - 1}))(seed) match {
             |      ${typeDef.schema.alternativeRequiredFields.zipWithIndex
               .map {
                 case (set, i) =>
                   s"case ${if (i == typeDef.schema.alternativeRequiredFields.size - 1) "_" else s"Some($i)"} => ${generateComposedFieldName(set.toSeq, "And")}CompoundSanitizer(seed)(entity)"
               }
               .mkString("\n      ")}
             |    }
             |""".stripMargin)
      ))
  }

  def generateCompoundSanitizer(
    typeDef: TypeDefinition,
    included: Set[String],
    excluded: Set[String],
    context: ScalaCodeGeneratorContext)(
    implicit typeResolver: TypeResolver,
    typeNameProvider: TypeNameProvider): Option[ScalaCode] =
    Some(
      ValueDefinition(
        name = s"${generateComposedFieldName(included.toSeq, "And")}CompoundSanitizer",
        returnType = "Update",
        body = Seq(s"""seed =>
                      |    entity =>
                      |      entity.copy(
                      |        ${included
                        .map(name => {
                          typeDef.schema.properties
                            .find(_.name == name)
                            .map(
                              prop =>
                                if (prop.boolean)
                                  s"""$name = true"""
                                else
                                  s"""$name = entity.$name.orElse(Generator.get(${generateValueGenerator(
                                    typeDef,
                                    prop,
                                    context,
                                    wrapOption = false)})(seed))${generateSanitizerSuffix(prop)}""")
                            .getOrElse("")
                        })
                        .mkString(",\n        ")}
                      |       ${excluded
                        .map(name => {
                          typeDef.schema.properties
                            .find(_.name == name)
                            .map(
                              prop =>
                                if (prop.boolean)
                                  s"""$name = false"""
                                else
                                  s"""$name = None""")
                            .getOrElse("")
                        })
                        .mkString(",\n        ", ",\n        ", "")}
                      |   )""".stripMargin)
      ))

  def generateSanitizerSuffix(
    schema: Schema)(implicit typeResolver: TypeResolver, typeNameProvider: TypeNameProvider): String =
    schema match {
      case a: ArraySchema =>
        s".map(_.map(${a.items.map(items => typeNameProvider.toTypeName(items.head)).getOrElse(typeResolver.any)}.sanitize(seed)))"
      case o: ObjectSchema => s".map(${typeNameProvider.toTypeName(o)}.sanitize(seed))"
      case o: OneOfAnyOfSchema =>
        if (o.variants.isEmpty) ""
        else if (o.variants.size == 1) generateSanitizerSuffix(o.variants.head)
        else
          s""".map(${typeNameProvider.toTypeName(o)}.sanitize(seed))"""
      case _ => ""
    }

  def generateGenerators(
    typeDef: TypeDefinition,
    fieldGenerators:      => String,
    fieldsInitialization: => String,
    context: ScalaCodeGeneratorContext): Seq[Option[ValueDefinition]] =
    Seq(
      context.generatorsOpt.map(_ =>
        ValueDefinition(
          name = "gen",
          returnType = s"Gen[${typeDef.name}]",
          body = Seq(
            if (typeDef.isInterface)
              s"Gen.oneOf[${typeDef.name}](${typeDef.subtypes
                .map(st => s"${st.name}.gen.map(_.asInstanceOf[${typeDef.name}])")
                .mkString(",\n  ")})"
            else if (fieldGenerators.isEmpty)
              s"Gen const ${typeDef.name}($fieldsInitialization)"
            else
              s"""for {
                 |    $fieldGenerators

                 |  } yield ${typeDef.name}($fieldsInitialization)""".stripMargin),
          modifier = Some("override")
      )))

  def generateValidator(
    typeDef: TypeDefinition,
    objectValidator: => String,
    context: ScalaCodeGeneratorContext
  ): Seq[Option[ValueDefinition]] =
    Seq(
      context.validatorsOpt.map(_ =>
        ValueDefinition(
          name = "validate",
          returnType = s"Validator[${typeDef.name}]",
          body = Seq(if (typeDef.isInterface && typeDef.subtypes.nonEmpty)
            s"{${typeDef.subtypes
              .map(subTypeDef => s"""case x: ${subTypeDef.name}   => ${subTypeDef.name}.validate(x)""")
              .mkString("\n    ")}}"
          else s"Validator($objectValidator)"),
          modifier = context.generatorsOpt.map(_ => "override")
      )))

  def generateSanitizers(
    typeDef: TypeDefinition,
    sanitizers:    => Seq[Option[ScalaCode]],
    sanitizerList: => String,
    context: ScalaCodeGeneratorContext): Seq[Option[ScalaCode]] =
    if (context.renderSanitizer) {
      if (typeDef.isInterface && typeDef.subtypes.nonEmpty)
        Seq(
          Some(
            ValueDefinition(
              name = "sanitizer",
              returnType = "Update",
              body = Seq(s"""seed => {${typeDef.subtypes
                .map(subTypeDef => s"""  case x: ${subTypeDef.name}   => ${subTypeDef.name}.sanitize(seed)(x)""")
                .mkString("\n")}}""")
            )),
          Some(
            ValueDefinition(
              name = "sanitizers",
              returnType = "Seq[Update]",
              body = Seq(s"Seq(sanitizer)")
            ))
        )
      else
        sanitizers ++ Seq(
          Some(
            ValueDefinition(
              name = "sanitizers",
              returnType = "Update",
              body = Seq(s"Seq($sanitizerList)"),
              modifier = Some("override"))))
    } else Seq()

  def generateCommonObjectDeclaration(context: ScalaCodeGeneratorContext): Seq[Option[ScalaCode]] =
    if (context.commonVals.isEmpty) Seq.empty
    else
      Seq(Some(Object(name = "Common", supertypes = Seq.empty, members = context.commonVals.map {
        case (value, name) => ValueDefinition(name = name, returnType = null, body = Seq(value))
      }.toSeq)))

  def generateJsonFormats(typeDef: TypeDefinition, context: ScalaCodeGeneratorContext): Seq[Option[ScalaCode]] =
    if (!context.renderPlayJson) Seq.empty
    else if (typeDef.isInterface)
      Seq(
        Some(
          ValueDefinition(
            name = "reads",
            returnType = s"Reads[${typeDef.name}]",
            body = Seq(
              s"""new Reads[${typeDef.name}] {
                 |      override def reads(json: JsValue): JsResult[${typeDef.name}] = {
                 |      ${typeDef.subtypes.zipWithIndex
                   .map {
                     case (subTypeDef, i) =>
                       s"""  val r$i = ${if (i > 0) s"r${i - 1}.orElse(" else ""}${subTypeDef.name}.formats.reads(json).flatMap(e => ${subTypeDef.name}.validate(e).fold(_ => JsError(), _ => JsSuccess(e)))${if (i > 0)
                         ")"
                       else ""}"""
                   }
                   .mkString("\n  ")}
                 |        r${typeDef.subtypes.size - 1}.orElse(aggregateErrors(JsError("Could not match json object to any variant of ${typeDef.name}, i.e. ${typeDef.subtypes
                   .map(_.name)
                   .mkString(", ")}"),${(for (i <- typeDef.subtypes.indices)
                   yield s"r$i").mkString(",")}))
                 |      }
                 |
                 |      private def aggregateErrors[T](errors: JsResult[T]*): JsError =
                 |        errors.foldLeft(JsError())((a, r) =>
                 |          r match {
                 |            case e: JsError => JsError(a.errors ++ e.errors)
                 |            case _          => a
                 |        })
                 |  }""".stripMargin),
            modifier = Some("implicit")
          )),
        Some(
          ValueDefinition(
            name = "writes",
            returnType = s"Writes[${typeDef.name}]",
            body =
              Seq(s"""new Writes[${typeDef.name}] {
                     |    override def writes(o: ${typeDef.name}): JsValue = o match {
                     |      ${typeDef.subtypes
                       .map(subTypeDef => s"""case x: ${subTypeDef.name}   => ${subTypeDef.name}.formats.writes(x)""")
                       .mkString("\n    ")}
                     |    }
                     |  }
          """.stripMargin),
            modifier = Some("implicit")
          ))
      )
    else
      Seq(
        Some(
          ValueDefinition(
            name = "formats",
            returnType = s"Format[${typeDef.name}]",
            body = Seq("Json.format[${typeDef.name}]"),
            modifier = Some("implicit"))))

  private def quoted(s: String): String = "\"\"\"" + s + "\"\"\""

  private def defined[A]: PartialFunction[Option[A], A] = { case Some(x) => x }

}
