import sbt.Keys._
import sbt._

//noinspection ScalaFileName
//noinspection ScalaDeprecation
object MyExampleBuild extends Build {

  lazy val project = Project (
    "simulino",
    file("."),
    settings = Defaults.defaultSettings ++
//      ScalatraPlugin.scalatraWithJRebel ++
//      scalateSettings ++
      Seq(
      resolvers += "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
      resolvers += "Akka Repo" at "http://repo.akka.io/repository"
//      scalateTemplateConfig in Compile <<= (sourceDirectory in Compile){ base =>
//        Seq(
//          TemplateConfig(
//            base / "webapp" / "WEB-INF" / "templates",
//            Seq.empty,  /* default imports should be added here */
//            Seq.empty,  /* add extra bindings here */
//            Some("templates")
//          )
//        )
//      }
    )
  )
}
