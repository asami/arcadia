package arcadia.view

import scala.util.control.NonFatal
import scala.xml._
import java.net.URI
import org.goldenport.record.v2._
import org.goldenport.xml.XmlUtils
import org.goldenport.exception.RAISE
import arcadia.model.Card

/*
 * @since   Nov.  8, 2017
 * @version Dec. 29, 2023
 * @author  ASAMI, Tomoharu
 */
case class ViewCard(v: Card, strategy: RenderStrategy) {
  val href: String = v.link.map(_.dataHref(strategy.renderContext).toString) getOrElse "#"
  lazy val imageTop: ViewPicture = ViewPicture.create(v.image_top, strategy)
  def property(key: String): String = property(key, "")
  def property(key: String, default: String): String = v.record.flatMap(_.getString(key)) getOrElse default

  def image_top: NodeSeq = imageTop.img
  def header: NodeSeq = XmlUtils.orEmptyNodeSeq(v.header.map(_.title(strategy.locale)))
  def footer: NodeSeq = XmlUtils.orEmptyNodeSeq(v.footer.map(_.title(strategy.locale)))
}

object ViewCard {
  def create(card: Card, strategy: RenderStrategy): ViewCard = ViewCard(card, strategy)
}
