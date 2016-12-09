name := "Monads"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
libraryDependencies += "org.scalaz" %% "scalaz-core"  % "7.2.8"
libraryDependencies += "org.scalaz" %% "scalaz-effect"  % "7.2.8"
libraryDependencies += "org.scalaz" %% "scalaz-concurrent"  % "7.2.8"
libraryDependencies += "org.typelevel" %% "cats" % "0.8.1"
libraryDependencies += "joda-time" % "joda-time" % "2.9.4"
libraryDependencies += "org.apache.pdfbox" % "pdfbox" % "1.1.0"


addCommandAlias("to", "; test-only 'euler.Pallindrom'")