name := "fromfields"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies += "com.beachape" %% "enumeratum" % "1.5.13"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"

libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.1" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")