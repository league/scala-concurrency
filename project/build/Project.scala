import sbt._

class Project(info: ProjectInfo)
extends DefaultProject(info)
{
  val scalazCore = "org.scalaz" %% "scalaz-core" % "6.0.1"
}
