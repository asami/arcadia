package arcadia.view

import scala.util.control.NonFatal
import java.net.URI
import org.goldenport.record.v2._
import org.goldenport.xml.XmlUtils
import org.goldenport.exception.RAISE
import arcadia.model.Card

/*
 * @since   Nov.  8, 2017
 * @version Nov.  8, 2017
 * @author  ASAMI, Tomoharu
 */
case class ViewCard(v: Card, strategy: RenderStrategy) {
  val href: String = v.link.map(_.dataHref(strategy.renderContext).toString) getOrElse "#"
  lazy val imageTop: ViewPicture = ViewPicture.create(v.image_top, strategy)
  def property(key: String): String = property(key, "")
  def property(key: String, default: String): String = v.record.flatMap(_.getString(key)) getOrElse default
}

object ViewCard {
  def create(card: Card, strategy: RenderStrategy): ViewCard = ViewCard(card, strategy)
}
