organization := "pt.tecnico.dsi"
name := "afs"
version := "0.0.1"

val javaVersion = "1.8"
initialize := {
  val current  = sys.props("java.specification.version")
  assert(current == javaVersion, s"Unsupported JDK: expected JDK $javaVersion installed, but instead got JDK $current.")
}
javacOptions ++= Seq("-source", javaVersion, "-target", javaVersion, "-Xlint")

scalaVersion := "2.11.8"
scalacOptions ++= Seq(
  "-deprecation",                   //Emit warning and location for usages of deprecated APIs.
  "-encoding", "UTF-8",             //Use UTF-8 encoding. Should be default.
  "-feature",                       //Emit warning and location for usages of features that should be imported explicitly.
  "-language:implicitConversions",  //Explicitly enables the implicit conversions feature
  "-unchecked",                     //Enable detailed unchecked (erasure) warnings
  "-Xfatal-warnings",               //Fail the compilation if there are any warnings.
  "-Xlint",                         //Enable recommended additional warnings.
  "-Yinline-warnings",              //Emit inlining warnings.
  "-Yno-adapted-args",              //Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
  "-Ywarn-dead-code"                //Warn when dead code is identified.
)

libraryDependencies ++= Seq(
  "work.martins.simon" %% "scala-expect" % "4.1.0",
  //Logging
  "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0",
  "ch.qos.logback" % "logback-classic" % "1.1.7",
  //Testing
  "org.scalatest" %% "scalatest" % "2.2.6" % "test",
  //Configuration
  "com.typesafe" % "config" % "1.3.0",
  //Dimensions mainly information (KB, MB, etc)
  "com.squants"  %% "squants"  % "0.6.2",
  //Time and dates
  "joda-time" % "joda-time" % "2.9.3",
  //See http://stackoverflow.com/questions/13856266/class-broken-error-with-joda-time-using-scala
  //as to why this library must be included
  "org.joda" % "joda-convert" % "1.8.1",

  "pt.tecnico.dsi" %% "kadmin" % "4.5.0" % "test"
)
resolvers += Resolver.typesafeRepo("releases")

autoAPIMappings := true
scalacOptions in (Compile,doc) ++= Seq("-groups", "-implicits", "-diagrams")

site.settings
site.includeScaladoc()
ghpages.settings
git.remoteRepo := s"git@github.com:ist-dsi/${name.value}.git"

licenses += "MIT" -> url("http://opensource.org/licenses/MIT")
homepage := Some(url(s"https://github.com/ist-dsi/${name.value}"))
scmInfo := Some(ScmInfo(homepage.value.get, git.remoteRepo.value))

publishMavenStyle := true
publishTo := Some(if (isSnapshot.value) Opts.resolver.sonatypeSnapshots else Opts.resolver.sonatypeStaging)
publishArtifact in Test := false

pomIncludeRepository := { _ => false }
pomExtra :=
  <developers>
    <developer>
      <id>Lasering</id>
      <name>Simão Martins</name>
      <url>https://github.com/Lasering</url>
    </developer>
    <developer>
      <id>botelhorui</id>
      <name>Rui Botelho</name>
      <url>https://github.com/botelhorui</url>
    </developer>
  </developers>

import ReleaseTransformations._

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  //runTest, how to run ./test.sh??
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  ReleaseStep(action = Command.process("publishSigned", _)),
  ReleaseStep(action = Command.process("ghpagesPushSite", _)),
  setNextVersion,
  commitNextVersion,
  ReleaseStep(action = Command.process("sonatypeRelease", _)),
  pushChanges
)

