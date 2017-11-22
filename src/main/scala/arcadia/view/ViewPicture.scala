package arcadia.view

import scala.util.control.NonFatal
import java.net.URI
import org.goldenport.record.v2._
import org.goldenport.xml.XmlUtils
import org.goldenport.exception.RAISE
import arcadia.model.Picture

/*
 * @since   Nov.  8, 2017
 * @version Nov. 22, 2017
 * @author  ASAMI, Tomoharu
 */
case class ViewPicture(v: Picture, strategy: RenderStrategy) {
  def src: String = v.img // (1280)
  def l: String = v.l // 1280
  def m: String = v.m // 640
  def s: String = v.s // 320
  def xs: String = v.xs // 160
  def alt: String = v.alt.map(_(strategy.locale)) getOrElse ""
}

object ViewPicture {
  def create(picture: Picture, strategy: RenderStrategy): ViewPicture = ViewPicture(picture, strategy)
  def create(picture: Option[Picture], strategy: RenderStrategy): ViewPicture =
    create(picture.getOrElse(strategy.noImagePicture), strategy)
}
