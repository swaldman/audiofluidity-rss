ThisBuild / publishTo := {
  if (isSnapshot.value) Some(Resolver.url("sonatype-snapshots", url("https://oss.sonatype.org/content/repositories/snapshots")))
  else Some(Resolver.url("sonatype-staging", url("https://oss.sonatype.org/service/local/staging/deploy/maven2")))
}

ThisBuild / organization := "com.mchange"
ThisBuild / version      := "0.0.4"
ThisBuild / scalaVersion := "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "audiofluidity-rss",
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.1.0",
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.1" % "test",
    testFrameworks += new TestFramework("utest.runner.Framework"),    
    pomExtra := pomExtraForProjectName_Apache2( name.value )
  )

def pomExtraForProjectName_Apache2( projectName : String ) = {
  <url>https://github.com/swaldman/{projectName}</url>
    <licenses>
      <license>
        <name>The Apache Software License, Version 2.0</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>https://github.com/swaldman/{projectName}
      </url>
      <connection>scm:git:git@github.com:swaldman/{projectName}.git</connection>
    </scm>
    <developers>
      <developer>
        <id>swaldman</id>
        <name>Steve Waldman</name>
        <email>swaldman@mchange.com</email>
      </developer>
    </developers>
}
