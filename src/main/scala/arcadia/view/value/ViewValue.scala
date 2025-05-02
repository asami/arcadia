package arcadia.view.value

import org.joda.time._
import org.goldenport.context.Showable
import org.goldenport.util.AnyUtils

/*
 * @since   May.  4, 2022
 * @version May.  4, 2022
 * @author  ASAMI, Tomoharu
 */
trait ViewValue extends Showable {
  override def toString() = print
}

case object ViewEmpty extends ViewValue {
  def print: String = ""
}

case class ViewString(string: String) extends ViewValue {
  def print = string
}

case class ViewNumber(number: Number) extends ViewValue {
  def print = AnyUtils.toString(number)
}

case class ViewBean(bean: Any) extends ViewValue {
  def print = AnyUtils.toString(bean)
}
