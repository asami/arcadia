package arcadia.model

/*
 * @since   Aug. 29, 2017
 * @version Sep.  4, 2017
 * @author  ASAMI, Tomoharu
 */
case class Transfer(
  start: Int,
  limit: Int,
  maxlimit: Int,
  length: Int,
  total: Option[Int]
)

object Transfer {
  val empty = Transfer(0, 0, 0, 0, None)
}
