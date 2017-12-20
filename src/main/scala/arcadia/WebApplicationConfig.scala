package arcadia

import scala.xml._
import scala.concurrent.duration._
import java.util.Locale
import java.net.URI
import com.typesafe.config.{Config, ConfigFactory}
import org.goldenport.record.v2.Record
import org.goldenport.i18n.I18NElement
import org.goldenport.xml.XhtmlUtils
import org.goldenport.json.JsonUtils.Implicits._
import org.goldenport.value._
import org.goldenport.util.HoconUtils.Implicits._

/*
 * @since   Aug. 12, 2017
 *  version Sep.  2, 2017
 *  version Oct. 27, 2017
 *  version Nov. 15, 2017
 * @version Dec. 21, 2017
 * @author  ASAMI, Tomoharu
 */
case class WebApplicationConfig(
  theme: Option[String],
  application_title: Option[I18NElement],
  logo_title: Option[I18NElement],
  logo_image: Option[URI],
  logo_url: Option[URI],
  copyright_title: Option[I18NElement],
  copyright_url: Option[URI],
  //
  feature_list: Option[WebApplicationConfig.FeatureList], // TopMenu, SideBar
  usecase_list: Option[WebApplicationConfig.UsecaseList], // navigator
  admin_list: Option[WebApplicationConfig.AdminList], // navigator
  info_list: Option[WebApplicationConfig.InfoList], // footer
  //
  singlePageApplication: Option[WebApplicationConfig.SinglePageApplication],
  http: Option[WebApplicationConfig.HttpConfig],
  route: Option[WebApplicationConfig.RouteConfig],
  //
  lifecycle: Option[WebApplicationConfig.LifecycleConfig],
  extend: Option[List[String]] // related feature: mixin
) {
  // def applicationTitle(locale: Locale): Node = application_title.flatMap(_.get(locale)) getOrElse Group(Nil)

  // def applicationLogo(locale: Locale): Node =
  //   XhtmlUtils.anchorOrImgOrTextOrNodeOrEmpty(logo_title.flatMap(_.get(locale)), logo_image, logo_url)

  def complement(rhs: WebApplicationConfig) = {
    import scalaz._, Scalaz._
    WebApplicationConfig(
      theme orElse rhs.theme,
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
      singlePageApplication orElse rhs.singlePageApplication,
      http orElse rhs.http,
      route orElse route, // TODO
      lifecycle orElse rhs.lifecycle,
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
    info_list.map(_.toRule),
    singlePageApplication.map(_.toRule),
    http.map(_.toRule),
    route.map(_.toRule) getOrElse Route.empty,
    Record.empty // TODO
  )

  def getAssets: Option[String] = lifecycle.
    flatMap(_.cdn.flatMap(_.assets)).
    map(_.toString)

  def getExpiresPeriod(kind: ExpiresKind): Option[FiniteDuration] =
    lifecycle.flatMap(_.expires.flatMap(_.getExpiresPeriod(kind)))
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

    lazy val toRule = WebApplicationRule.Page(name, title, icon)
  }

  case class FeatureList(
    page: List[Page],
    feature_list: Option[FeatureList]
  ) {
    lazy val toRule: WebApplicationRule.FeatureList = WebApplicationRule.FeatureList(
      page.map(_.toRule),
      feature_list.map(_.toRule)
    )
  }

  case class UsecaseList(
    page: List[Page],
    usecase_list: Option[UsecaseList]
  ) {
    lazy val toRule: WebApplicationRule.UsecaseList = WebApplicationRule.UsecaseList(
      page.map(_.toRule),
      usecase_list.map(_.toRule)
    )
  }

  case class AdminList(
    page: List[Page],
    admin_list: Option[AdminList]
  ) {
    lazy val toRule: WebApplicationRule.AdminList = WebApplicationRule.AdminList(
      page.map(_.toRule),
      admin_list.map(_.toRule)
    )
  }

  case class InfoList(
    page: List[Page],
    info_list: Option[InfoList]
  ) {
    lazy val toRule: WebApplicationRule.InfoList = WebApplicationRule.InfoList(
      page.map(_.toRule),
      info_list.map(_.toRule)
    )
  }

  case class SinglePageApplication(
    base_uri: Option[List[URI]]
  ) {
    lazy val toOption = base_uri.map(x => Some(this)) getOrElse None
    lazy val toRule: WebApplicationRule.SinglePageApplication = WebApplicationRule.SinglePageApplication(
      base_uri getOrElse Nil
    )
  }

  case class HttpConfig(
    cookieSecureKind: Option[WebApplicationRule.CookieSecureKind],
    loginMaxAge: Option[FiniteDuration],
    accessMaxAge: Option[FiniteDuration]
  ) {
    def isEmpty = cookieSecureKind.isEmpty

    def complement(rhs: HttpConfig) =
      copy(cookieSecureKind = cookieSecureKind orElse rhs.cookieSecureKind)

    def toRule = WebApplicationRule.Http(
      cookieSecureKind getOrElse WebApplicationRule.Http.default.cookieSecureKind,
      loginMaxAge getOrElse WebApplicationRule.Http.default.loginMaxAge,
      accessMaxAge getOrElse WebApplicationRule.Http.default.accessMaxAge
    )

    def toOption: Option[HttpConfig] =
      if (isEmpty)
        None
      else
        Some(this)
  }

  case class RouteConfig(
    dummy: String
  ) {
    def toRule: Route = Route.empty
  }

  case class LifecycleConfig(
    expires: Option[ExpiresConfig],
    cdn: Option[CdnConfig]
  ) {
    lazy val isEmpty = expires.isEmpty && cdn.isEmpty
    lazy val toOption: Option[LifecycleConfig] =
      if (isEmpty) None else Some(this)
  }
  object LifecycleConfig {
    val empty = LifecycleConfig(
      None, // ExpiresConfig(None, None, None, None), // (1.day, 3.hour, 1.hour, 1.minute),
      None // CdnConfig(None, None, None)
    )
  }

  case class ExpiresConfig(
    assets: Option[FiniteDuration],
    staticPage: Option[FiniteDuration],
    commonPage: Option[FiniteDuration], // TODO
    privatePage: Option[FiniteDuration]
  ) {
    val isEmpty = assets.isEmpty && staticPage.isEmpty && commonPage.isEmpty && privatePage.isEmpty
    lazy val toOption: Option[ExpiresConfig] = if (isEmpty) None else Some(this)

    def getExpiresPeriod(kind: ExpiresKind): Option[FiniteDuration] =
      kind match {
        case AssetsExpires => assets
        case StaticPageExpires => staticPage
        case StablePageExpires => commonPage
        case AgilePageExpires => None
        case FragilePageExpires => None
        case DynamicPageExpires => None
        case PrivatePageExpires => None
        case NoCacheExpires => None
      }
  }

  case class CdnConfig(
    assets: Option[URI],
    staticPage: Option[URI],
    commonPage: Option[URI]
  ) {
    val isEmpty = assets.isEmpty && staticPage.isEmpty && commonPage.isEmpty
    lazy val toOption: Option[CdnConfig] = if (isEmpty) None else Some(this)
  }

  def create(name: String): WebApplicationConfig = WebApplicationConfig(
    None,
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
  import WebApplicationRule._

  // TODO Migrate to goldenport-scala-lib
  // See com.everforth.lib.swagger.Specification
  class ValueInstanceFormat[T <: ValueInstance](klass: ValueClass[T]) extends Format[T] {
    def reads(json: JsValue): JsResult[T] = json match {
      case JsString(s) => klass.get(s).
          map(JsSuccess(_)).
          getOrElse(JsError(s"ValueInstanceFormat($json)"))
      case _ => JsError(s"ValueInstanceFormat($json)")
    }
    def writes(o: T): JsValue = JsString(o.toString)
  }

  implicit object CookieSecureKindFormat extends ValueInstanceFormat[CookieSecureKind](CookieSecureKind)

  implicit val PageFormat = Json.format[Page]
  implicit val FeatureListFormat = Json.format[FeatureList]
  implicit val UsecaseListFormat = Json.format[UsecaseList]
  implicit val AdminListFormat = Json.format[AdminList]
  implicit val InfoListFormat = Json.format[InfoList]
  implicit val SinglePageApplicationFormat = Json.format[SinglePageApplication]
  implicit val ExpiresConfigFormat = Json.format[ExpiresConfig]
  implicit val CdnConfigFormat = Json.format[CdnConfig]
  implicit val HttpConfigFormat = Json.format[HttpConfig]
  implicit val RouteConfigFormat = Json.format[RouteConfig]
  implicit val LifecycleConfigFormat = Json.format[LifecycleConfig]
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
    val expires = ExpiresConfig(
      c.getDurationOption("expires.assets"),
      c.getDurationOption("expires.static_page"),
      c.getDurationOption("expires.common_page"),
      c.getDurationOption("expires.private_page")
    )
    val cdn = CdnConfig(
      c.getUriOption("cdn.assets"),
      c.getUriOption("cdn.static_page"),
      c.getUriOption("cdn.common_page")
    )
    val spa = SinglePageApplication(
      c.getUriListOption("singlePageApplication.base_uri")
    )
    val http = HttpConfig(
      c.getStringOption("http.cookie.secure").map(CookieSecureKind(_)),
      c.getDurationOption("http.login.maxAge"),
      c.getDurationOption("http.access.maxAge")
    )
    val route = Some(RouteConfig("???"))
    val lifecycle = LifecycleConfig(expires.toOption, cdn.toOption)
    WebApplicationConfig(
      c.getStringOption("theme"),
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
      spa.toOption,
      http.toOption,
      route,
      lifecycle.toOption,
      c.getEagerStringListOption("extend")
    )
  }
}
