name := "pusca"

version := "1.0.0-SNAPSHOT"

organization := "ch.inventsoft"

scalaVersion := "2.9.1"



libraryDependencies += "com.typesafe.config" % "config" % "0.2.0"

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.9.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1"

libraryDependencies += "junit" % "junit" % "4.4"


resolvers ++= Seq(
	"Inventsoft Release Repository" at "http://mavenrepo.inventsoft.ch/repo",
  "Typesafe Release Repository" at "http://repo.typesafe.com/typesafe/releases",
	"Inventsoft Snapshot Repository" at "http://mavenrepo.inventsoft.ch/snapshot-repo")

publishTo <<= (version) { version: String =>
  val nexus = "http://nexus.scala-tools.org/content/repositories/"
  if (version.trim.endsWith("SNAPSHOT")) Some(Resolver.ssh("/inventsoft/dev/mavenrepo/snapshot-repo", "foxtrot.inventsoft.ch") as("msiegenthaler")) 
  else                                   Some(Resolver.ssh("/inventsoft/dev/mavenrepo/repo", "foxtrot.inventsoft.ch") as("msiegenthaler"))
}
