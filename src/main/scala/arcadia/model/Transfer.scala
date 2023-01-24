package arcadia.model

import arcadia.context.Query

/*
 * @since   Aug. 29, 2017
 *  version Sep.  4, 2017
 * @version Jan. 24, 2023
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

  def create(q: Query, ps: Seq[_]): Transfer = Transfer(
    q.start,
    q.limit,
    q.maxlimit,
    ps.length,
    None
  )
}
