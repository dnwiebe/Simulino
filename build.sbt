name := "Simulino"

version := "0.1"

scalaVersion := "2.11.6"

libraryDependencies += "com.fasterxml.jackson.core" % "jackson-core" % "2.5.3"
libraryDependencies += "com.fasterxml.jackson.core" % "jackson-databind" % "2.5.3"
libraryDependencies += "org.scalatra" %% "scalatra" % "2.4.0.RC1"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.3" % "runtime"
libraryDependencies += "org.eclipse.jetty" % "jetty-webapp" % "9.2.10.v20150310"
//noinspection ScalaUnnecessaryParentheses
libraryDependencies += "javax.servlet" % "javax.servlet-api" % "3.1.0" % "provided;test"// artifacts (Artifact("javax.servlet", "jar", "jar"))
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
libraryDependencies += "org.mockito" % "mockito-all" % "1.10.19" % "test"
