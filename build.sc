import mill._
import scalalib._
import coursier.maven.MavenRepository
import $ivy.`com.lihaoyi::mill-contrib-bloop:$MILL_VERSION`

object sculpt extends ScalaModule {
  def repositories = super.repositories ++ Seq(
    MavenRepository("https://oss.sonatype.org/content/repositories/releases"),
    MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")
  )

  def ivyDeps = Agg(
    ivy"edu.berkeley.cs::chisel3:3.2-SNAPSHOT",
    ivy"edu.berkeley.cs::chisel-testers2:0.1-SNAPSHOT"
  )

  def scalacOptions = Seq(
    "-Xsource:2.11"
  )

  def scalaVersion = "2.12.10"
}

