name := "VGit"

version := "0.1"

organization := "net.fosd"

scalaVersion := "2.11.7"


libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "4.0.1.201506240215-r"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "net.fosd" %% "vregex" % "0.1"


libraryDependencies += "com.github.axel22" %% "scalameter" % "0.5-M2"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
