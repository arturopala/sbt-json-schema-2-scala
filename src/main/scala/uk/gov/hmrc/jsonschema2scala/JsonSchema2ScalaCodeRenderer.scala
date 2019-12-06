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

import uk.gov.hmrc.jsonschema2scala.JsonSchema._
import uk.gov.hmrc.jsonschema2scala.Names._
import uk.gov.hmrc.jsonschema2scala.ScalaCode._

object JsonSchema2ScalaCodeRenderer extends JsonSchema2CodeRenderer with KnownFieldGenerators with CodeRendererUtils {

  override def typeName(definition: Definition): String = {
    val n = if (definition.isRef) pathToName(definition) else definition.name
    if (n.isEmpty) definition.name else firstCharUppercase(normalize(n))
  }

  def render(
    className: String,
    typeDef: TypeDefinition,
    options: JsonSchema2ScalaOptions,
    description: String): Seq[ScalaCode] = {

    val context = ScalaCodeRendererContext(typeDef.definition, options)

    val code: Seq[Option[ScalaCode]] = Seq(
      Some(Package(options.packageName)),
      context.generatorsOpt.map(_ => Import("org.scalacheck", List("Arbitrary", "Gen"))),
      context.generatorsOpt.map(_ => WildcardImport("play.api.libs.json")),
      Some(BlockComment(s"""
                           | ---------------------------------------------------
                           | THIS FILE HAS BEEN GENERATED - DO NOT MODIFY IT !!!
                           |          CHANGE THE JSON SCHEMA IF NEEDED
                           | ---------------------------------------------------
                           | $description
                           | Structure:
                           |  ${generateTypesMap(typeDef)}
        """.stripMargin))
    ) ++ generateTypeDefinition(typeDef, isTopLevel = true, context)

    code.collect(defined)
  }

  def generateTypesMap(typeDef: TypeDefinition, level: Int = 1): String =
    s"${typeDef.name}${typeDef.nestedTypes
      .filter(!_.definition.isRef || level == 1)
      .map(t => "\n  *  " + ("-  " * level) + generateTypesMap(t, level + 1))
      .mkString("")}"

  def generateTypeDefinition(
    typeDef: TypeDefinition,
    isTopLevel: Boolean,
    context: ScalaCodeRendererContext): Seq[Option[ScalaCode]] = {

    lazy val classFields = generateClassFields(typeDef)
    lazy val propertyValidators = generatePropertyValidators(typeDef.definition, context)
    lazy val objectValidator = generateObjectValidator(typeDef.definition, context)
    lazy val fieldGenerators = generateFieldGenerators(typeDef.definition, context)
    lazy val fieldsInitialization = generateGenFieldsInitialization(typeDef.definition)
    lazy val sanitizers = generateSanitizers(typeDef.definition, context)
    lazy val sanitizerList = generateSanitizerList(typeDef.definition)
    lazy val customObject = if (isTopLevel) generateCustomObjectDeclaration(context) else Seq.empty
    lazy val nestedTypesDefinitions: Seq[Option[ScalaCode]] = typeDef.nestedTypes
      .filter(!_.definition.isRef || isTopLevel)
      .flatMap(t => generateTypeDefinition(t, isTopLevel = false, context))

    lazy val objectMembersCode: Seq[Option[ScalaCode]] = if (isTopLevel) {
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
    } else Seq.empty

    lazy val generatorsCode: Seq[Option[ScalaCode]] =
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

    lazy val validatorCode: Seq[Option[ScalaCode]] =
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

    lazy val sanitizersCode: Seq[Option[ScalaCode]] =
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

    lazy val jsonFormatsCode: Seq[Option[ScalaCode]] =
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

    // -----------------------------------------
    //    CASE CLASS AND OBJECT TEMPLATE
    // -----------------------------------------

    val classCode2 =
      if (typeDef.isInterface)
        s"""sealed trait ${typeDef.name} {${generateInterfaceMethods(typeDef)}}""".stripMargin
      else
        s"""case class ${typeDef.name}(
        |  $classFields${if (isTopLevel && context.renderGenerators)
          s""",
             |  id: Option[String] = None
             |) extends Record${if (typeDef.hasInterfaces) " with " else ""}${generateClassInterfaces(typeDef)} {
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
          s")${if (typeDef.hasInterfaces) " extends " else ""}${generateClassInterfaces(typeDef)} {"}
        |
        |  ${if (context.renderBuilders) generateBuilderMethods(typeDef) else ""}
        |}"""

    val classCode =
      if (typeDef.isInterface)
        Trait(typeDef.name, Seq.empty, generateInterfaceMethods(typeDef), Some("sealed"))
      else
        CaseClass(
          name = typeDef.name,
          parameters = classFields ++ (if (isTopLevel && context.renderGenerators)
                                         Seq(Param("id", "Option[String] = None"))
                                       else Seq.empty),
          supertypes = (if (isTopLevel && context.renderGenerators) Seq("Record") else Seq.empty) ++ generateClassInterfaces(
            typeDef),
          members = if (context.renderBuilders) generateBuilderMethods(typeDef) else Seq.empty
        )

    val objectCode =
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
          customObject).flatten.collect(defined)
      ).asOption

    val objectImport =
      if (isTopLevel) objectCode.map(_ => WildcardImport(s"${context.packageName}.${typeDef.name}"))
      else None

    Seq(objectImport, Some(classCode), objectCode)
  }

  def generateClassInterfaces(typeDef: TypeDefinition): Seq[String] =
    if (typeDef.interfaces.isEmpty) Seq.empty
    else typeDef.interfaces.map(it => s"${it.prefix}${it.name}").distinct

  def generateClassFields(typeDef: TypeDefinition): Seq[Param] =
    typeDef.definition.properties
      .take(22)
      .map(
        prop =>
          Param(
            safeName(prop.name),
            typeOf(prop, typeDef.prefix),
            if (typeDef.interfaces.exists(_.interfaceMethods.exists(_._1 == prop.name)))
              Some("override val")
            else None))

  def generateInterfaceMethods(typeDef: TypeDefinition): Seq[ScalaCode] =
    if (!typeDef.isInterface) Seq.empty
    else
      typeDef.interfaceMethods.map {
        case (name, typeOf) => MethodDefinition(safeName(name), Seq.empty, typeOf, Seq.empty, None)
      }.toSeq

  def generateFieldGenerators(definition: ObjectDefinition, context: ScalaCodeRendererContext): String =
    definition.properties
      .filter(_.isMandatory)
      .take(22)
      .map(prop => s"""${variableName(prop)} <- ${generateValueGenerator(prop, context)}""".stripMargin)
      .mkString("\n    ")

  def generateGenFieldsInitialization(definition: ObjectDefinition): String =
    definition.properties
      .filter(_.isMandatory)
      .take(22)
      .map(prop => s"""${safeName(prop.name)} = ${variableName(prop)}""".stripMargin)
      .mkString("\n    ", ",\n    ", "\n  ")

  def generateBuilderMethods(typeDef: TypeDefinition): Seq[ScalaCode] =
    typeDef.definition.properties
      .take(22)
      .flatMap(prop => {
        val propType = typeOf(prop, typeDef.prefix, defaultValue = false)
        Seq(
          MethodDefinition(
            name = s"with${firstCharUppercase(normalize(prop.name))}",
            parameters = Seq(Param(safeName(prop.name), propType)),
            returnType = typeDef.name,
            body = Seq(s"copy(${safeName(prop.name)} = ${safeName(prop.name)})")
          ),
          MethodDefinition(
            name = s"modify${firstCharUppercase(normalize(prop.name))}",
            parameters = Seq(Param("pf", s"PartialFunction[$propType, $propType]")),
            returnType = typeDef.name,
            body = Seq(
              s"if (pf.isDefinedAt(${safeName(prop.name)})) copy(${safeName(prop.name)} = pf(${safeName(prop.name)})) else this")
          )
        )
      })

  def generateValueGenerator(
    property: Definition,
    context: ScalaCodeRendererContext,
    wrapOption: Boolean = true): String = {
    val gen = knownFieldGenerators(property.name)
      .orElse(knownFieldGenerators(pathLastPart(property)))
      .getOrElse(property match {
        case s: StringDefinition =>
          s.customGenerator.getOrElse(if (s.enum.isDefined) {
            if (s.enum.get.size == 1)
              s"""Gen.const("${s.enum.get.head}")"""
            else
              s"""Gen.oneOf(${context.commonReference(s"Seq(${s.enum.get.mkString("\"", "\",\"", "\"")})")})"""
          } else if (s.pattern.isDefined)
            s"""Generator.regex(${context.commonReference(quoted(s.pattern.get))})"""
          else if (s.minLength.isDefined || s.maxLength.isDefined)
            s"""Generator.stringMinMaxN(${s.minLength.getOrElse(1)},${s.maxLength.getOrElse(256)})"""
          else "Generator.stringMaxN(256)")

        case n: NumberDefinition =>
          n.customGenerator.getOrElse((n.minimum, n.maximum, n.multipleOf) match {
            case (Some(min), Some(max), mlt) => s"Generator.chooseBigDecimal($min,$max,$mlt)"
            case (Some(min), None, mlt)      => s"Generator.chooseBigDecimal($min,100000000,$mlt)"
            case (None, Some(max), mlt)      => s"Generator.chooseBigDecimal(0,$max,$mlt)"
            case _                           => "Gen.const(BigDecimal(0))"
          })

        case b: BooleanDefinition => "Generator.booleanGen"
        case a: ArrayDefinition   => s"Generator.nonEmptyListOfMaxN(1,${generateValueGenerator(a.item, context, false)})"
        case o: ObjectDefinition  => s"${typeName(o)}.gen"

        case o: OneOfDefinition =>
          if (o.variants.isEmpty) "???"
          else if (o.variants.size == 1) generateValueGenerator(o.variants.head, context)
          else
            o.variants.head match {
              case _: ObjectDefinition => s"${typeName(o)}.gen"
              case _ =>
                s"Gen.oneOf[${typeName(o)}](${o.variants
                  .map(v => s"${generateValueGenerator(v, context)}.map(_.asInstanceOf[${typeOf(v, "")}])")
                  .mkString(",\n  ")})"
            }
        case e: ExternalDefinition => s"${typeName(e)}.gen"
      })

    val genWithConstraints = property match {
      case s: StringDefinition =>
        val withMinLength = s.minLength.map(minLength => s"""$gen.suchThat(_.length>=$minLength)""").getOrElse(gen)
        val withMaxLength =
          s.maxLength.map(maxLength => s"""$withMinLength.suchThat(_.length<=$maxLength)""").getOrElse(withMinLength)
        withMaxLength
      case _ => gen
    }

    if (!property.isMandatory && wrapOption) s"""Generator.optionGen($genWithConstraints)""" else genWithConstraints
  }

  def generatePropertyValidators(
    definition: ObjectDefinition,
    context: ScalaCodeRendererContext): Seq[Option[ScalaCode]] =
    if (!context.renderValidators) Seq.empty
    else
      definition.properties
        .take(22)
        .map(prop => generateValueValidator(prop, context, extractProperty = false).map((prop, _)))
        .collect {
          case Some((prop, validator)) =>
            Some(
              ValueDefinition(
                name = s"${prop.name}Validator",
                returnType = s"Validator[${typeOf(prop, "", defaultValue = false)}]",
                body = Seq(validator),
                modifier = None
              ))
        }

  def generateObjectValidator(definition: ObjectDefinition, context: ScalaCodeRendererContext): String = {
    val propertyValidatorsCalls = definition.properties
      .take(22)
      .map(prop => generateValueValidatorCall(prop, context))
      .collect { case Some(validator) => s"""$validator""".stripMargin }
    val validators =
      if (definition.alternatives.isEmpty) propertyValidatorsCalls
      else
        propertyValidatorsCalls :+
          s"""  checkIfOnlyOneSetIsDefined(${definition.alternatives
            .map(_.map(a => {
              definition.properties.find(_.name == a).map(prop => s"_.$a${if (prop.isBoolean) ".asOption" else ""}").get
            }).mkString("Set(", ",", ")"))
            .mkString("Seq(", ",", ")")},"${definition.alternatives
            .map(_.mkString("{", ",", "}"))
            .mkString("[", ",", "]")}")"""
    validators.mkString(",\n  ")
  }

  def generateValueValidator(
    property: Definition,
    context: ScalaCodeRendererContext,
    isMandatory: Boolean = false,
    extractProperty: Boolean = true): Option[String] = {
    val propertyReference = if (extractProperty) s"_.${property.name}" else "_"
    val propertyExtractor = if (extractProperty) s"_.${property.name}" else "identity"
    property match {
      case s: StringDefinition =>
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

      case n: NumberDefinition =>
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

      case a: ArrayDefinition =>
        val itemValidator: Option[String] = (a.item match {
          case o: ObjectDefinition => Some(s"""${typeName(o)}.validate""")
          case x                   => generateValueValidator(x, context)
        }).map(vv =>
          if (property.isMandatory || isMandatory) s""" checkEach($propertyExtractor, $vv)"""
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

      case _: ObjectDefinition =>
        if (property.isMandatory || isMandatory)
          Some(s""" checkProperty($propertyExtractor, ${typeName(property)}.validate)""")
        else Some(s"""  checkIfSome($propertyExtractor, ${typeName(property)}.validate)""")

      case o: OneOfDefinition =>
        if (o.variants.isEmpty) None
        else if (o.variants.size == 1) generateValueValidator(o.variants.head, context, o.isMandatory)
        else
          o.variants.head match {
            case _: ObjectDefinition =>
              if (property.isMandatory || isMandatory)
                Some(s""" checkProperty($propertyExtractor, ${typeName(property)}.validate)""")
              else Some(s"""  checkIfSome($propertyExtractor, ${typeName(property)}.validate)""")
            case _ =>
              generateValueValidator(o.variants.head, context, o.isMandatory)
          }

      case _: BooleanDefinition => None
      case _: ExternalDefinition =>
        if (property.isMandatory || isMandatory)
          Some(s""" checkProperty($propertyExtractor, ${typeName(property)}.validate)""")
        else Some(s"""  checkIfSome($propertyExtractor, ${typeName(property)}.validate)""")
    }
  }

  def generateValueValidatorCall(
    property: Definition,
    context: ScalaCodeRendererContext,
    isMandatory: Boolean = false): Option[String] =
    property match {
      case d: Definition if d.shallBeValidated =>
        Some(s"""  checkProperty(_.${safeName(property.name)}, ${property.name}Validator)""")
      case _ => None
    }

  def generateSanitizerList(definition: ObjectDefinition): String = {
    val simpleSanitizerList = definition.properties
      .filter(p => !(p.isMandatory && p.isPrimitive) && !definition.alternatives.exists(_.contains(p.name)))
      .take(22)
      .map(prop => s"${prop.name}Sanitizer")
    val sanitizerList =
      if (definition.alternatives.isEmpty) simpleSanitizerList
      else
        simpleSanitizerList :+ s"${generateComposedFieldName(definition.alternatives.map(_.head), "Or")}AlternativeSanitizer"
    sanitizerList.mkString(",\n  ")
  }

  def generateComposedFieldName(parts: Seq[String], sep: String): String =
    (parts.head +: parts.tail.map(p => p.take(1).toUpperCase + p.drop(1))).mkString(sep)

  def generateSanitizers(definition: ObjectDefinition, context: ScalaCodeRendererContext): Seq[Option[ScalaCode]] = {
    val simpleSanitizers: Seq[Option[ScalaCode]] = definition.properties
      .take(22)
      .toList
      .map(prop =>
        if (prop.isMandatory) {
          if (prop.isPrimitive) None
          else
            Some(
              ValueDefinition(
                name = s"${prop.name}Sanitizer",
                returnType = "Update",
                body = Seq(s"""seed => entity =>
                              |    entity.copy(${safeName(prop.name)} = ${prop match {
                                case o: ObjectDefinition =>
                                  s"${typeName(o)}.sanitize(seed)(entity.${safeName(prop.name)})"
                                case a: ArrayDefinition if !a.item.isPrimitive =>
                                  s"entity.${safeName(prop.name)}.map(item => ${typeName(a.item)}.sanitize(seed)(item))"
                                case o: OneOfDefinition if o.variants.nonEmpty && !o.variants.head.isPrimitive =>
                                  if (o.variants.size == 1)
                                    s"${typeName(o.variants.head)}.sanitize(seed)(entity.${safeName(prop.name)})"
                                  else
                                    o.variants
                                      .map(v => s"case x:${typeName(v)} => ${typeName(v)}.sanitize(seed)(x)")
                                      .mkString(s"entity.${safeName(prop.name)} match {\n  ", "\n  ", "\n}")
                                case _ => s"entity.${safeName(prop.name)}"
                              }})
         """.stripMargin)
              ))
        } else
          Some(ValueDefinition(
            name = s"${prop.name}Sanitizer",
            returnType = "Update",
            body = Seq(if (prop.isPrimitive) {
              if (prop.shallBeValidated)
                s"""seed => entity =>
                   |    entity.copy(${prop.name} = ${prop.name}Validator(entity.${prop.name}).fold(_ => None, _ => entity.${prop.name})
                   |      .orElse(Generator.get(${generateValueGenerator(prop, context, wrapOption = false)})(seed)))
                   """.stripMargin
              else
                s"""seed => entity =>
                   |    entity.copy(${prop.name} = Generator.get(${generateValueGenerator(
                     prop,
                     context,
                     wrapOption = false)})(seed))
           """.stripMargin
            } else
              s"""seed => entity =>
                 |    entity.copy(${safeName(prop.name)} = entity.${safeName(prop.name)}
                 |      .orElse(Generator.get(${generateValueGenerator(prop, context, wrapOption = false)})(seed))${generateSanitizerSuffix(
                   prop)})
           """.stripMargin)
          )))

    if (definition.alternatives.isEmpty) simpleSanitizers
    else simpleSanitizers ++ generateAlternativeSanitizers(definition, context)
  }

  def generateAlternativeSanitizers(
    definition: ObjectDefinition,
    context: ScalaCodeRendererContext): Seq[Option[ScalaCode]] = {

    val compoundSanitizers: Seq[Option[ScalaCode]] = definition.alternatives.toList
      .map(set =>
        generateCompoundSanitizer(definition, set, definition.alternatives.filterNot(_ == set).reduce(_ ++ _), context))

    compoundSanitizers :+ Some(
      ValueDefinition(
        name = s"${generateComposedFieldName(definition.alternatives.map(_.head), "Or")}AlternativeSanitizer",
        returnType = "Update",
        body = Seq(
          s"""seed => entity =>
             |          ${definition.alternatives
               .map(set =>
                 s"if(entity.${set.head}.isDefined) ${generateComposedFieldName(set.toSeq, "And")}CompoundSanitizer(seed)(entity)")
               .mkString("\nelse      ")}
             |          else Generator.get(Gen.chooseNum(0,${definition.alternatives.size - 1}))(seed) match {
             |      ${definition.alternatives.zipWithIndex
               .map {
                 case (set, i) =>
                   s"case ${if (i == definition.alternatives.size - 1) "_" else s"Some($i)"} => ${generateComposedFieldName(set.toSeq, "And")}CompoundSanitizer(seed)(entity)"
               }
               .mkString("\n      ")}
             |    }
             |""".stripMargin)
      ))
  }

  def generateCompoundSanitizer(
    definition: ObjectDefinition,
    included: Set[String],
    excluded: Set[String],
    context: ScalaCodeRendererContext): Option[ScalaCode] =
    Some(
      ValueDefinition(
        name = s"${generateComposedFieldName(included.toSeq, "And")}CompoundSanitizer",
        returnType = "Update",
        body = Seq(s"""seed =>
                      |    entity =>
                      |      entity.copy(
                      |        ${included
                        .map(name => {
                          definition.properties
                            .find(_.name == name)
                            .map(
                              prop =>
                                if (prop.isBoolean)
                                  s"""$name = true"""
                                else
                                  s"""$name = entity.$name.orElse(Generator.get(${generateValueGenerator(
                                    prop,
                                    context,
                                    wrapOption = false)})(seed))${generateSanitizerSuffix(prop)}""")
                            .getOrElse("")
                        })
                        .mkString(",\n        ")}
                      |       ${excluded
                        .map(name => {
                          definition.properties
                            .find(_.name == name)
                            .map(
                              prop =>
                                if (prop.isBoolean)
                                  s"""$name = false"""
                                else
                                  s"""$name = None""")
                            .getOrElse("")
                        })
                        .mkString(",\n        ", ",\n        ", "")}
                      |   )""".stripMargin)
      ))

  def generateSanitizerSuffix(definition: Definition): String = definition match {
    case a: ArrayDefinition  => s".map(_.map(${typeName(a.item)}.sanitize(seed)))"
    case o: ObjectDefinition => s".map(${typeName(o)}.sanitize(seed))"
    case o: OneOfDefinition =>
      if (o.variants.isEmpty) ""
      else if (o.variants.size == 1) generateSanitizerSuffix(o.variants.head)
      else
        s""".map(${typeName(o)}.sanitize(seed))"""
    case _ => ""
  }

  def generateCustomObjectDeclaration(context: ScalaCodeRendererContext): Seq[Option[ScalaCode]] =
    if (context.commonVals.isEmpty) Seq.empty
    else
      Seq(Some(Object(name = "Common", supertypes = Seq.empty, members = context.commonVals.map {
        case (value, name) => ValueDefinition(name = name, returnType = null, body = Seq(value))
      }.toSeq)))

}
