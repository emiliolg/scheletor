import org.scalajs.core.tools.linker.ModuleKind
import sbt.Keys.{libraryDependencies, resolvers}

name := "scheletor"

val settings = Common.settings ++ Common.publish ++ Seq(
    organization := "org.mule.scheletor",
    name := "scheletor",
    version := "0.1.0-SNAPSHOT",
    libraryDependencies ++= Seq(
        "org.mule.common" %%% "scala-common" % "0.1.3",
        "org.mule.syaml"  %%% "syaml"        % "0.4.1-SNAPSHOT",
        "org.scalatest"   %%% "scalatest"    % "3.0.0" % Test
    ),
    resolvers ++= List(Common.releases, Common.snapshots, Resolver.mavenLocal),
    credentials ++= Common.credentials()
)

lazy val scheletor = crossProject
  .in(file("."))
  .settings(settings: _*)
  .jvmSettings(
      // JVM-specific settings here
  )
  .jsSettings(
      // JS-specific settings here
      scalaJSModuleKind := ModuleKind.CommonJSModule
  )

lazy val scheletorJVM = scheletor.jvm
lazy val scheletorJS  = scheletor.js
