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

package uk.gov.hmrc.jsonschema2scala.generator

import scala.util.matching.Regex

trait KnownFieldGenerators {

  val phoneNumber: Regex = "^(.*?)(?:tele)?phonenumber(.*)$".r
  val mobileNumber: Regex = "^(.*?)mobilenumber(.*)$".r
  val faxNumber: Regex = "^(.*?)faxnumber(.*)$".r
  val emailAddress: Regex = "^(.*?)email(?:address)?(.*)$".r
  val addressLine: Regex = "^(.*?)(?:address)?line(1|2|3|4)(.*)$".r
  val postalCode: Regex = "^(.*?)post(?:al)?code(.*)$".r
  val organisationName: Regex = "^(.*?)(?:organisation|company)name(.*)$".r
  val lastName: Regex = "^(.*?)(?:lastname|surname)(.*)$".r
  val firstName: Regex = "^(.*?)firstname(.*)$".r
  val agencyName: Regex = "^(.*?)agen(?:t|cy)name(.*)$".r
  val date: Regex = "^(.*?)date(?:string)?(.*)$".r

  val knownFieldGenerators: String => Option[String] = s =>
    Option(s.toLowerCase match {
      case "safeid"               => "Generator.safeIdGen"
      case "agentreferencenumber" => "Generator.arnGen"
      case "nino"                 => "Generator.ninoNoSpacesGen"
      case "mtdbsa"               => "Generator.mtdbsaGen"
      case "vrn"                  => "Generator.vrnGen"
      case "utr"                  => "Generator.utrGen"
      case "eori"                 => "Generator.eoriGen"
      case date(a, b)             => s"Generator.dateYYYYMMDDGen${withPerturb(a, b)}"
      case phoneNumber(a, b) =>
        s"Generator.ukPhoneNumber${withPerturb(a, b)}"
      case mobileNumber(a, b) =>
        s"Generator.ukPhoneNumber${withPerturb(a, b)}"
      case faxNumber(a, b) =>
        s"Generator.ukPhoneNumber${withPerturb(a, b)}"
      case emailAddress(a, b) =>
        s"Generator.emailGen${withPerturb(a, b)}"
      case addressLine(a, n, b)   => s"Generator.address4Lines35Gen.map(_.line$n)${withPerturb(a, b)}"
      case postalCode(a, b)       => s"Generator.postcode${withPerturb(a, b)}"
      case "tradingname"          => "Generator.tradingNameGen"
      case organisationName(a, b) => s"Generator.company${withPerturb(a, b)}"
      case lastName(a, b)         => s"Generator.surname${withPerturb(a, b)}"
      case firstName(a, b)        => s"Generator.forename()${withPerturb(a, b)}"
      case "middlename"           => s"Generator.forename()${withPerturb("middle")}"
      case agencyName(a, b)       => s"UserGenerator.agencyNameGen${withPerturb(a, b)}.map(_.take(40))"
      case _                      => null
    })

  def withPerturb(p: String*): String =
    if (p.forall(_.isEmpty)) "" else s""".variant("${p.filterNot(_.isEmpty).mkString("-")}")"""
}
