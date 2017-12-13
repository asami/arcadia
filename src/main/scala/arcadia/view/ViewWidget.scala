package arcadia.view

import scala.util.control.NonFatal
import java.net.URI
import org.goldenport.record.v2._
import org.goldenport.xml.XmlUtils
import org.goldenport.exception.RAISE
import arcadia.model.WidgetModel

/*
 * @since   Dec. 13, 2017
 * @version Dec. 13, 2017
 * @author  ASAMI, Tomoharu
 */
case class ViewWidget(model: WidgetModel, strategy: RenderStrategy) {
  def name = model.name
  def expression = model.expression
  def get(key: String) = expression.get(key)
  def take(key: String) = expression.take(key)
}
