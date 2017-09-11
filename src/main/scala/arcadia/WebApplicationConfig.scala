package arcadia

import scala.xml._
import java.util.Locale
import java.net.URI
import com.typesafe.config.{Config, ConfigFactory}
import org.goldenport.i18n.I18NElement
import org.goldenport.xml.XhtmlUtils
import org.goldenport.util.HoconUtils.Implicits._

/*
 * @since   Aug. 12, 2017
 *  version Aug. 30, 2017
 * @version Sep.  2, 2017
 * @author  ASAMI, Tomoharu
 */
case class WebApplicationConfig(
  application_title: Option[I18NElement],
  logo_title: Option[I18NElement],
  logo_image: Option[URI],
  logo_url: Option[URI],
  copyright_title: Option[I18NElement],
  copyright_url: Option[URI],
  feature_list: Option[WebApplicationConfig.FeatureList], // TopMenu, SideBar
  usecase_list: Option[WebApplicationConfig.UsecaseList], // navigator
  admin_list: Option[WebApplicationConfig.AdminList], // navigator
  info_list: Option[WebApplicationConfig.InfoList], // footer
  extend: Option[List[String]] // related feature: mixin
) {
  // def applicationTitle(locale: Locale): Node = application_title.flatMap(_.get(locale)) getOrElse Group(Nil)

  // def applicationLogo(locale: Locale): Node =
  //   XhtmlUtils.anchorOrImgOrTextOrNodeOrEmpty(logo_title.flatMap(_.get(locale)), logo_image, logo_url)

  def complement(rhs: WebApplicationConfig) = {
    import scalaz._, Scalaz._
    WebApplicationConfig(
      application_title orElse rhs.application_title,
      logo_title orElse rhs.logo_title,
      logo_image orElse rhs.logo_image,
      logo_url orElse rhs.logo_url,
      copyright_title orElse rhs.copyright_title,
      copyright_url orElse rhs.copyright_url,
      feature_list orElse rhs.feature_list,
      usecase_list orElse rhs.usecase_list,
      admin_list orElse rhs.admin_list,
      info_list orElse rhs.info_list,
      extend |+| rhs.extend
    )
  }

  // def complements(xs: Seq[WebApplicationConfig]) = xs./:(this)(_ complement _)

  def toRule: WebApplicationRule = WebApplicationRule(
    application_title,
    logo_title,
    logo_image,
    logo_url,
    copyright_title,
    copyright_url,
    feature_list.map(_.toRule),
    usecase_list.map(_.toRule),
    admin_list.map(_.toRule),
    info_list.map(_.toRule)
  )
}

object WebApplicationConfig {
  val empty = WebApplicationConfig(
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None
  )

  case class Page(
    name: String,
    title: Option[I18NElement] = None,
    icon: Option[I18NElement] = None
  ) {
    def title(locale: Locale): NodeSeq = title.flatMap(_.get(locale)) getOrElse {
      XhtmlUtils.title(name)
    }

    def toRule = WebApplicationRule.Page(name, title, icon)
  }

  case class FeatureList(
    page: List[Page],
    feature_list: Option[FeatureList]
  ) {
    def toRule: WebApplicationRule.FeatureList = WebApplicationRule.FeatureList(
      page.map(_.toRule),
      feature_list.map(_.toRule)
    )
  }

  case class UsecaseList(
    page: List[Page],
    usecase_list: Option[UsecaseList]
  ) {
    def toRule: WebApplicationRule.UsecaseList = WebApplicationRule.UsecaseList(
      page.map(_.toRule),
      usecase_list.map(_.toRule)
    )
  }

  case class AdminList(
    page: List[Page],
    admin_list: Option[AdminList]
  ) {
    def toRule: WebApplicationRule.AdminList = WebApplicationRule.AdminList(
      page.map(_.toRule),
      admin_list.map(_.toRule)
    )
  }

  case class InfoList(
    page: List[Page],
    info_list: Option[InfoList]
  ) {
    def toRule: WebApplicationRule.InfoList = WebApplicationRule.InfoList(
      page.map(_.toRule),
      info_list.map(_.toRule)
    )
  }

  def create(name: String): WebApplicationConfig = WebApplicationConfig(
    Some(I18NElement(name)),
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None
  )

  import play.api.libs.json._
  import play.api.libs.functional.syntax._

  def parse(s: String): WebApplicationConfig = {
    val a = s.trim
    if (a.startsWith("{"))
      _parse_json(a)
    else
      _parse_hocon(a)
  }

  import org.goldenport.json.JsonUtils._
  import org.goldenport.json.JsonUtils.Implicits._

  implicit val PageFormat = Json.format[Page]
  implicit val FeatureListFormat = Json.format[FeatureList]
  implicit val UsecaseListFormat = Json.format[UsecaseList]
  implicit val AdminListFormat = Json.format[AdminList]
  implicit val InfoListFormat = Json.format[InfoList]
  implicit val WebApplicationConfigFormat = Json.format[WebApplicationConfig]

  private def _parse_json(s: String): WebApplicationConfig = {
    val json = Json.parse(s)
    Json.fromJson[WebApplicationConfig](json) match {
      case JsSuccess(s, _) => s
      case m: JsError => throw new IllegalArgumentException(m.toString) // XXX
    }
  }

  private def _parse_hocon(s: String): WebApplicationConfig = {
    val c = ConfigFactory.parseString(s)
    WebApplicationConfig(
      c.getI18NElementOption("application_title"),
      c.getI18NElementOption("logo_title"),
      c.getUriOption("logo_image"),
      c.getUriOption("logo_url"),
      c.getI18NElementOption("copyright_title"),
      c.getUriOption("logo_url"),
      None,
      None,
      None,
      None,
      c.getEagerStringListOption("extend")
    )
  }
}
