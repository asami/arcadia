package arcadia.view

import scala.util.control.NonFatal
import scala.xml._
import java.net.URI
import org.goldenport.record.v2._
import org.goldenport.xml.XmlUtils
import org.goldenport.exception.RAISE
import arcadia.model.Picture

/*
 * @since   Nov.  8, 2017
 *  version Nov. 22, 2017
 * @version Dec. 29, 2023
 * @author  ASAMI, Tomoharu
 */
sealed trait ViewPicture {
  def src: String
  def l: String
  def m: String
  def s: String
  def xs: String
  def alt: String

  def img: NodeSeq = <img src={src}/>
}
object ViewPicture {
  case class UriViewPicture(v: Picture.UriPicture, strategy: RenderStrategy) extends ViewPicture {
    def src: String = v.img // (1280)
    def l: String = v.l // 1280
    def m: String = v.m // 640
    def s: String = v.s // 320
    def xs: String = v.xs // 160
    def alt: String = v.alt.map(_(strategy.locale)) getOrElse ""
  }

  def create(picture: Picture, strategy: RenderStrategy): ViewPicture = picture match {
    case m: Picture.UriPicture => UriViewPicture(m, strategy)
    case m: Picture.IconPicture => RAISE.notImplementedYetDefect
  }
  def create(picture: Option[Picture], strategy: RenderStrategy): ViewPicture =
    create(picture.getOrElse(strategy.noImagePicture), strategy)
}
