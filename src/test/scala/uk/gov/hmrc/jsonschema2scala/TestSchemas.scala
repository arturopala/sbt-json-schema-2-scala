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

import uk.gov.hmrc.jsonschema2scala.schema.SchemaSource

import scala.util.Try

trait TestSchemas {

  lazy val allSchemas: Seq[SchemaSource] = unverifiedTestSchemas ++ verifiedTestSchemas

  lazy val unverifiedTestSchemas: Seq[SchemaSource] = unverifiedTestSchemaList
    .map(readSchemaSource)

  lazy val verifiedTestSchemas: Seq[SchemaSource] = verifiedTestSchemaList
    .map(readSchemaSource)

  import uk.gov.hmrc.jsonschema2scala.utils.TryOps._

  def readSchemaSource: String => SchemaSource = { filename =>
    Try(classOf[SchemaReaderSpec].getResourceAsStream(f"/schemas/$filename"))
      .flatMap { is =>
        Try(SchemaSource(filename, is))
      }
      .logError(e => {
        sys.error(s"Creating test schema source $filename failed with $e")
      })
      .get
  }

  val verifiedTestSchemaList = Seq(
    "E01.schema.json",
    "E02.schema.json",
    "E03.schema.json",
    "E04.schema.json",
    "E05.schema.json",
    "E06.schema.json",
    "E07.schema.json",
    "E08.schema.json",
    "E09.schema.json",
    "E10.schema.json",
    "E11.schema.json",
    "E12.schema.json",
    "E13.schema.json",
    "E14.schema.json",
    "E15.schema.json",
    "E16.schema.json",
    "apple-app-site-association.json",
    "appsettings.json",
    "appsscript.json",
    "appveyor.json",
    "angular.schema.json",
    "asmdef.json",
    "avro-avsc.json",
    "azure-iot-edge-deployment-1.0.json",
    "azure-iot-edge-deployment-2.0.json",
    "azure-iot-edge-deployment-template-1.0.json",
    "azure-iot-edge-deployment-template-2.0.json",
    "babelrc.json",
    "backportrc.json",
    "band-manifest.json",
    "BizTalkServerApplicationSchema.json",
    "bootstraprc.json",
    "bower.json",
    "bowerrc.json",
    "bozr.json",
    "bukkit-plugin.json",
    "bundleconfig.json",
    "bungee-plugin.json",
    "chutzpah.json",
    "clasp.json",
    "cloud-sdk-pipeline-config-schema.json",
    "cloudbuild.json",
    "coffeelint.json",
    "commands.json",
    "compilerdefaults.json",
    "component.json",
    "composer.json",
    "config.json",
    "content-security-policy-report-2.json",
    "contribute.json",
    "cosmos-config.json",
    "creatomic.json",
    "csscomb.json",
    "datalogic-scan2deploy-android.json",
    "datalogic-scan2deploy-ce.json",
    "github-workflow.json",
    "grunt-copy-task.json",
    "grunt-cssmin-task.json",
    "cryproj.52.schema.json",
    "cryproj.53.schema.json",
    "cryproj.54.schema.json",
    "cryproj.55.schema.json",
    "cryproj.dev.schema.json",
    "cryproj.json",
    "csslintrc.json",
    "dependabot.json",
    "docfx.json",
    "dotnetcli.host.json",
    "drone.json",
    "drush.site.yml.json",
    "dss-2.0.0.json",
    "electron-builder.json",
    "epr-manifest.json",
    "eslintrc.json",
    "esquio.json",
    "fabric.mod.json",
    "feed.json",
    "foxx-manifest.json",
    "function.json",
    "geojson.json",
    "github-action.json",
    "gitlab-ci.json",
    "global.json",
    "grunt-clean-task.json",
    "grunt-jshint-task.json",
    "grunt-task.json",
    "grunt-watch-task.json",
    "haxelib.json",
    "host.json",
    "host-meta.json",
    "huskyrc.json",
    "htmlhint.json",
    "jsbeautifyrc-nested.json",
    "jsonld.json",
    "ksp-ckan.json",
    "imageoptimizer.json",
    "install.json",
    "jasonette.json",
    "jdt.json",
    "jekyll.json",
    "jsbeautifyrc.json",
    "jsconfig.json",
    "jscsrc.json",
    "jshintrc.json",
    "jsinspectrc.json",
    "json-api-1.0.json",
    "kustomization.json",
    "launchsettings.json",
    "lerna.json",
    "libman.json",
    "lintstagedrc.schema.json",
    "lsdlschema-0.7.json",
    "lsdlschema-1.0.json",
    "lsdlschema-1.2.json",
    "mta.json",
    "mycode.json",
    "modernizrrc.json",
    "mtad.json",
    "mtaext.json",
    "ninjs-1.0.json",
    "ninjs-1.1.json",
    "ninjs-1.2.json",
    "nlu.json",
    "nodemon.json",
    "now.json",
    "npm-link-up.json",
    "npmpackagejsonlintrc.json",
    "nswag.json",
    "nuget-project.json",
    "nuget-project-3.3.0.json",
    "ocelot.json",
    "omnisharp.json",
    "openfin.json",
    "package.json",
    "package.manifest.json",
    "package.manifest-7.0.0.json",
    "package.manifest-8.0.0.json",
    "pattern.json",
    "pocketmine-plugin.json",
    "prettierrc.json",
    "prettierrc-1.8.2.json",
    "prisma.json",
    "project.json",
    "project-1.0.0-rc2.json",
    "pubspec.json",
    "pyrseas-0.8.json",
    "renovate.json",
    "resume.json",
    "sarif.json",
    "sarif-1.0.0.json",
    "sarif-2.0.0.json",
    "sarif-2.0.0-csd.2.beta.2018-10-10.json",
    "sarif-2.0.0-csd.2.beta.2019-01-09.json",
    "sarif-2.0.0-csd.2.beta.2019-01-24.json",
    "sarif-2.1.0-rtm.0.json",
    "sarif-2.1.0-rtm.1.json",
    "sarif-2.1.0-rtm.2.json",
    "sarif-2.1.0-rtm.3.json",
    "sarif-2.1.0-rtm.4.json",
    "sarif-external-property-file.json",
    "sarif-external-property-file-2.1.0-rtm.0.json",
    "sarif-external-property-file-2.1.0-rtm.1.json",
    "sarif-external-property-file-2.1.0-rtm.2.json",
    "sarif-external-property-file-2.1.0-rtm.3.json",
    "sarif-external-property-file-2.1.0-rtm.4.json",
    "schema-catalog.json",
    "schema-draft-v4.json",
    "swagger-2.0.json",
    "settings.job.json",
    "solidaritySchema.json",
    "sourcehut-build.json",
    "sourcemap-v3.json",
    "sprite.json",
    "stylintrc.json",
    "templatesources.json",
    "toolinfo.1.1.0.json",
    "ts-force-config.json",
    "tsconfig.json",
    "tsd.json",
    "tsdrc.json",
    "tslint.json",
    "typedoc.json",
    "typewiz.json",
    "typings.json",
    "typingsrc.json",
    "up.json",
    "vega-lite.json",
    "vs-2017.3.host.json",
    "vsconfig.json",
    "vsext.json",
    "vsix-manifestinjection.json",
    "vsix-publish.json",
    "vsls.json",
    "vss-extension.json",
    "web-manifest.json",
    "webextension.json",
    "webjob-publish-settings.json",
    "webjobs-list.json",
    "vs-nesting.json",
    "web-types.json",
    "xunit.runner.schema.json",
    "ansible-stable-2.0.json"
    //"ansible-stable-2.1.json",
    //"ansible-stable-2.2.json"
  )

  val unverifiedTestSchemaList = Seq(
    //"ansible-stable-2.3.json",
    //"ansible-stable-2.4.json",
    //"ansible-stable-2.5.json",
    //"ansible-stable-2.6.json",
    //"ansible-stable-2.7.json",
    "debugsettings.json",
    "json-patch.json",
    "licenses.1.json",
    "project-1.0.0-beta3.json",
    "project-1.0.0-beta4.json",
    "project-1.0.0-beta5.json",
    "project-1.0.0-beta6.json",
    "project-1.0.0-beta8.json",
    "project-1.0.0-rc1.json",
    "resjson.json",
    "template.json",
    "behat.json",
    "chrome-manifest.json",
    "circleciconfig.json",
    "codecov.json",
    "compilerconfig.json",
    "hemtt-0.6.2.json",
    "lsdlschema.json",
    "mimetypes.json",
    "opspec-io-0.1.7.json",
    "phraseapp.json",
    "prometheus.json",
    "prometheus.rules.json",
    "proxies.json",
    "schema-org-action.json",
    "schema-org-contact-point.json",
    "schema-org-place.json",
    "schema-org-thing.json",
    "stylelintrc.json",
    "travis.json",
    "ui5-manifest.json",
    "vega.json"
  )
}
