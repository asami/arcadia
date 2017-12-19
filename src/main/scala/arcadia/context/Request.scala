package arcadia.context

import org.goldenport.record.v2.Record

/*
 * @since   Dec. 19, 2017
 * @version Dec. 19, 2017
 * @author  ASAMI, Tomoharu
 */
case class Request(
  pathname: String,
  operationName: String,
  method: String, // TODO
  query: Record,
  form: Record
) {
}
