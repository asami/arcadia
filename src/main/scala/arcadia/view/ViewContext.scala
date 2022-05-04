package arcadia.view

import play.api.libs.json.JsValue
import org.goldenport.record.v3.Record
import arcadia._
import arcadia.context.{Query => CQuery, _}
import arcadia.domain._
import arcadia.model._

/*
 * @since   Apr. 30, 2022
 * @version May.  1, 2022
 * @author  ASAMI, Tomoharu
 */
case class ViewContext(
  engine: ViewEngine,
  parcel: Parcel
) {
  def isMatch(e: ViewEngine, p: Parcel) = engine == e && parcel == p
}
