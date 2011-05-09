import sbt._

class PuscaProject(info: ProjectInfo) extends DefaultProject(info) with AutoCompilerPlugins {
  override def packageSrcJar = defaultJarPath("-sources.jar")
  val sourceArtifact = Artifact.sources(artifactID)
  override def packageToPublishActions = super.packageToPublishActions ++ Seq(packageSrc)

  val inventsoftReleases = "Inventsoft Release Repository" at "http://mavenrepo.inventsoft.ch/repo"
  val inventsoftSnapshots = "Inventsoft Snapshot Repository" at "http://mavenrepo.inventsoft.ch/snapshot-repo"
  val jbossRepo = "JBoss public repository" at "http://repository.jboss.org/nexus/content/groups/public-jboss/"
  override def managedStyle = ManagedStyle.Maven
  val publishTo = Resolver.sftp("Inventsoft Publish", "foxtrot.inventsoft.ch", "/inventsoft/dev/mavenrepo/snapshot-repo")
  Credentials(Path.userHome / ".ivy2" / ".credentials", log)
}
