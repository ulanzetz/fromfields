name := "fromfields"

organization := "com.github.ulanzetz"

publishTo := {
  if (isSnapshot.value) {
    Some(Opts.resolver.sonatypeSnapshots)
  } else sonatypePublishToBundle.value
}

licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))

publishMavenStyle := true

version := "1.0.0"

scalaVersion := "2.13.1"

libraryDependencies += "com.beachape" %% "enumeratum" % "1.5.13"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"

libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.1" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")
