name := "minimapper"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.12.3"

scalacOptions ++= Seq(
  "-encoding", "UTF-8",   // source files are in UTF-8
  "-deprecation",         // warn about use of deprecated APIs
  "-unchecked",           // warn about unchecked type parameters
  "-feature",             // warn about misused language features
  "-language:higherKinds",// allow higher kinded types without `import scala.language.higherKinds`
  "-Xfatal-warnings",     // turn compiler warnings into errors
  "-Ypartial-unification" // allow the compiler to unify type constructors of different arities
)

libraryDependencies ++= Seq(
  "org.typelevel"              %% "cats-core"     % "1.0.0-RC1",
  "io.circe"                   %% "circe-core"    % "0.9.0-M2",
  "io.circe"                   %% "circe-generic" % "0.9.0-M2",
  "io.circe"                   %% "circe-parser"  % "0.9.0-M2",
  "com.github.julien-truffaut" %% "monocle-core"  % "1.4.0",
  "com.github.julien-truffaut" %% "monocle-macro" % "1.4.0",
  "com.chuusai"                %% "shapeless"     % "2.3.2",
  "org.scalacheck"             %% "scalacheck"    % "1.13.4" % Test,
  "org.scalatest"              %% "scalatest"     % "3.0.4"  % Test
)

// For partially applying type constructors:
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

// For Monocle's @Lenses annotation:
addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)
