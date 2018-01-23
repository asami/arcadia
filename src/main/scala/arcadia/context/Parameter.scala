package arcadia.context

import scala.xml._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.goldenport.json.JsonUtils.Implicits._
import org.goldenport.i18n.I18NString
import org.goldenport.record.v2._
import Column.Form

/*
 * @since   Jan. 22, 2018
 * @version Jan. 22, 2018
 * @author  ASAMI, Tomoharu
 */
case class Parameter(
  name: String,
  datatype: Option[DataType] = None,
  multiplicity: Option[Multiplicity] = None,
  label: Option[I18NString] = None,
  placeholder: Option[I18NString] = None,
  value: Option[String] = None,
  readonly: Option[Boolean] = None,
  hidden: Option[Boolean] = None
) {
  def toInput(cssclass: String): NodeSeq = <input type={take_datatype} name={name} value={take_value} class={cssclass}></input>

  protected def take_datatype: String =
    if (hidden.getOrElse(false))
      "hidden"
    else
      "text" // TODO

  protected def take_value: String = value getOrElse ""
}

object Parameter {
  object json {
    import Schema.json._
    implicit val ParameterFormat = Json.format[Parameter]
  }
}
