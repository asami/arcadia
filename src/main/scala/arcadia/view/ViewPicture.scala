package arcadia.view

import scala.util.control.NonFatal
import java.net.URI
import org.goldenport.record.v2._
import org.goldenport.xml.XmlUtils
import org.goldenport.exception.RAISE
import arcadia.model.Picture

/*
 * @since   Nov.  8, 2017
 * @version Nov.  8, 2017
 * @author  ASAMI, Tomoharu
 */
case class ViewPicture(v: Picture, strategy: RenderStrategy) {
  def src: String = v.src.toString
  def alt: String = v.alt.map(_(strategy.locale)) getOrElse ""
}

object ViewPicture {
  def create(picture: Picture, strategy: RenderStrategy): ViewPicture = ViewPicture(picture, strategy)
  def create(picture: Option[Picture], strategy: RenderStrategy): ViewPicture = ViewPicture(strategy.noImagePicture, strategy)
}
