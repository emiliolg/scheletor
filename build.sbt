import sbt.Keys.{libraryDependencies, resolvers}
import sbtcrossproject.CrossPlugin.autoImport.crossProject

val shareSettings = Common.settings ++ Common.publish ++ Seq(
    organization := "org.mule.scheletor",
    version := "0.1.0",
    resolvers ++= List(Common.releases, Common.snapshots, Resolver.mavenLocal),
    credentials ++= Common.credentials(),
    libraryDependencies ++= Seq(
        "org.mule.common" %%% "scala-common" % "0.3.3",
        "org.scalatest"   %%% "scalatest"    % "3.0.0" % Test
    )
)

lazy val scheletor = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings(Seq(name := "scheletor") ++ shareSettings)

lazy val scheletorJVM = scheletor.jvm
lazy val scheletorJS  = scheletor.js

lazy val syaml = crossProject(JSPlatform, JVMPlatform)
  .settings(
      shareSettings ++ Seq(
          name := "scheletor-syaml",
          libraryDependencies += "org.mule.syaml" %%% "syaml" % "0.4.2"
      ))
  .dependsOn(scheletor)

lazy val syamlJVM = syaml.jvm
lazy val syamlJS  = syaml.js
