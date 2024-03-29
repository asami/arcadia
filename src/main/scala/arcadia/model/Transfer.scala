package arcadia.model

import arcadia.context.Query

/*
 * @since   Aug. 29, 2017
 *  version Sep.  4, 2017
 *  version Jan. 24, 2023
 *  version Sep. 30, 2023
 *  version Oct. 31, 2023
 * @version Nov.  4, 2023
 * @author  ASAMI, Tomoharu
 */
case class Transfer(
  offset: Int,
  limit: Int,
  maxlimit: Int,
  length: Int,
  total: Option[Int]
)

object Transfer {
  val empty = Transfer(0, 0, 0, 0, None)

  def create(q: Query, ps: Seq[_]): Transfer = {
    val total = if (ps.length < q.limit)
      Some(q.offset + ps.length)
    else
      None
    Transfer(
      q.offset,
      q.limit,
      q.maxlimit,
      ps.length,
      total
    )
  }
}
