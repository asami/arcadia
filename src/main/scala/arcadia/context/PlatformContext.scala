package arcadia.context

import java.io.File

/*
 * @since   Aug. 29, 2017
 * @version Aug. 30, 2017
 * @author  ASAMI, Tomoharu
 */
trait PlatformContext {
  def getDevelopDirectory: Option[File]
  def createTempDirectory(): File
}

object PlatformContext {
  val empty = ???
}
