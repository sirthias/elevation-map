enablePlugins(AutomateHeaderPlugin)

version := "1.0"
scalaVersion := "2.13.1"
organization := "fine.lines.software"
homepage := Some(new URL("http://fine.lines.software"))

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:_",
  "-unchecked",
  "-target:jvm-1.8",
  "-Xlint:_,-missing-interpolator",
  "-Xfatal-warnings",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ybackend-parallelism", "8",
  "-Ywarn-unused:imports,-patvars,-privates,-locals,-implicits,-explicits",
  "-Ycache-macro-class-loader:last-modified",
)

scalacOptions in (Compile, console) ~= (_ filterNot(o => o.contains("warn") || o.contains("Xlint")))
scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value
scalacOptions in (Compile, doc) += "-no-link-warnings"

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

sourcesInBase := false

// file headers
headerLicense := Some(HeaderLicense.Custom("(C) 2015-2019 fine.lines.software. All rights reserved."))

// reformat main and test sources on compile
scalafmtOnCompile := true

testFrameworks += new TestFramework("utest.runner.Framework")

fork in run := true

libraryDependencies ++= Seq(
  "io.bullet"               %% "borer-core"       % "1.1.0",
  "io.bullet"               %% "borer-derivation" % "1.1.0",
  "org.typelevel"           %% "cats-core"        % "2.0.0",
  "com.lihaoyi"             %% "utest"            % "0.7.1" % "test",
  "io.jenetics"             % "jpx"               % "1.5.3",
  "org.tukaani"             % "xz"                % "1.8",
  "org.apache.commons"      % "commons-imaging"   % "1.0-alpha1"
)