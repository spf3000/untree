val Specs2Version = "4.0.2"


lazy val root = (project in file("."))
  .settings(
    organization := "io.github.spf3000",
    name := "untree",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.12.4",
    resolvers += Resolver.sonatypeRepo("releases"),
    libraryDependencies ++= Seq(
      "org.specs2"      %% "specs2-core"         % Specs2Version % "test",
      "com.slamdata" 	%% "matryoshka-core"	 % "0.18.3",
      "org.typelevel"   %% "spire"               % "0.14.1"
      )
      )

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")
