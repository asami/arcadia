package arcadia.view.value

import org.joda.time._
import org.goldenport.context.Showable

/*
 * @since   May.  2, 2022
 * @version May.  4, 2022
 * @author  ASAMI, Tomoharu
 */
case class ViewTime(
  dateTime: DateTime,
  print: String
) extends ViewValue {
}
