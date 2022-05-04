package arcadia.view.value

import org.joda.time._
import org.goldenport.context.Showable

/*
 * @since   May.  1, 2022
 * @version May.  4, 2022
 * @author  ASAMI, Tomoharu
 */
case class ViewDateTime(
  dateTime: DateTime,
  print: String
) extends Showable {
}
