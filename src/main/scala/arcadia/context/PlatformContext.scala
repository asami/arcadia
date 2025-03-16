package arcadia.context

import java.io.File
import org.goldenport.Platform
import org.goldenport.value._

/*
 * @since   Aug. 29, 2017
 *  version Aug. 30, 2017
 *  version Jan. 29, 2023
 * @version Mar. 11, 2025
 * @author  ASAMI, Tomoharu
 */
trait PlatformContext {
  def mode: PlatformContext.Mode
  def getDevelopDirectory: Option[File]
  def createTempDirectory(): File
}

object PlatformContext {
  val develop = StandalonePlatformContext.develop

  sealed trait Mode extends NamedValueInstance {
  }
  object Mode extends EnumerationClass[Mode] {
    val elements = Vector(Production, Develop, Test, Demo)

    case object Production extends Mode {
      val name = "production"
    }
    case object Develop extends Mode {
      val name = "develop"
    }
    case object Test extends Mode {
      val name = "test"
    }
    case object Demo extends Mode {
      val name = "demo"
    }
  }

  case class StandalonePlatformContext(
    mode: PlatformContext.Mode,
    getDevelopDirectory: Option[File],
    createTempDirectoryf: () => File
  ) extends PlatformContext {
    def createTempDirectory(): File = createTempDirectoryf()
  }
  object StandalonePlatformContext {
    val develop = StandalonePlatformContext(
      Mode.Develop,
      None,
      createTempDirectory
    )

    def createTempDirectory() = Platform.createTempDirectory("arcadia")
  }
}
