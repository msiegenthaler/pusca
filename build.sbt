name := "pusca"

version := "1.0.0-SNAPSHOT"

organization := "ch.inventsoft"

scalaVersion := "2.9.1"



libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.9.1"



resolvers ++= Seq(
	"Inventsoft Release Repository" at "http://mavenrepo.inventsoft.ch/repo",
	"Inventsoft Snapshot Repository" at "http://mavenrepo.inventsoft.ch/snapshot-repo")

publishTo <<= (version) { version: String =>
  val nexus = "http://nexus.scala-tools.org/content/repositories/"
  if (version.trim.endsWith("SNAPSHOT")) Some(Resolver.ssh("/inventsoft/dev/mavenrepo/snapshot-repo", "foxtrot.inventsoft.ch") as("msiegenthaler")) 
  else                                   Some(Resolver.ssh("/inventsoft/dev/mavenrepo/repo", "foxtrot.inventsoft.ch") as("msiegenthaler"))
}
