package arcadia

import scala.xml._
import scala.concurrent.duration._
import java.util.Locale
import java.net.URI
import com.typesafe.config.{Config, ConfigFactory}
import org.goldenport.record.v2.Record
import org.goldenport.i18n.I18NElement
import org.goldenport.i18n.I18NString
import org.goldenport.xml.XhtmlUtils
import org.goldenport.value._
import org.goldenport.util.StringUtils
import org.goldenport.util.HoconUtils.Implicits._
import arcadia.model.SubmitKind
import arcadia.view._

/*
 * @since   Aug. 12, 2017
 *  version Oct. 27, 2017
 *  version Nov. 15, 2017
 *  version Dec. 20, 2017
 *  version Mar. 13, 2018
 * @version Aug.  6, 2018
 * @author  ASAMI, Tomoharu
 */
case class WebApplicationRule(
  application_title: Option[I18NElement],
  logo_title: Option[I18NElement],
  logo_image: Option[URI],
  logo_url: Option[URI],
  copyright_title: Option[I18NElement],
  copyright_url: Option[URI],
  feature_list: Option[WebApplicationRule.FeatureList], // TopMenu, SideBar
  usecase_list: Option[WebApplicationRule.UsecaseList], // navigator
  admin_list: Option[WebApplicationRule.AdminList], // navigator
  info_list: Option[WebApplicationRule.InfoList], // footer
  singlePageApplication: Option[WebApplicationRule.SinglePageApplication],
  http: Option[WebApplicationRule.Http],
  route: Route,
  extraPages: WebApplicationRule.Pages,
  properties: Record
) {
  import WebApplicationRule.{Pages, Page}

  def applicationTitle(locale: Locale): NodeSeq = application_title.flatMap(_.get(locale)) getOrElse Group(Nil)

  def applicationLogo(locale: Locale): NodeSeq =
    XhtmlUtils.anchorOrImgOrTextOrNodeOrEmpty(logo_title.flatMap(_.get(locale)), logo_image, logo_url)

  def complement(rhs: WebApplicationRule) = {
    import scalaz._, Scalaz._
    WebApplicationRule(
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
      (http, rhs.http) match {
        case (Some(l), Some(r)) => Some(l complement r)
        case (Some(l), None) => Some(l)
        case (None, Some(r)) => Some(r)
        case (None, None) => None
      },
      route.complement(rhs.route),
      pages.complement(rhs.pages),
      properties.complements(rhs.properties)
    )
  }

  def complements(xs: Seq[WebApplicationRule]) = xs./:(this)(_ complement _)

  def submitLabel(kind: SubmitKind): I18NString = kind.label

  def getString(key: String): Option[String] = properties.getString(key)

  lazy val pages: Pages = (
    Pages.create(feature_list.map(_.page)).
      complement(Pages.create(usecase_list.map(_.page))).
      complement(Pages.create(admin_list.map(_.page))).
      complement(Pages.create(info_list.map(_.page))).
      complement(extraPages)
  )

  def getPage(view: View): Option[Page] = pages.get(view)
}

object WebApplicationRule {
  val DEFAULT_LOGIN_MAXAGE = 14.days
  val DEFAULT_ACCESS_MAXAGE = (10*365).days

  val empty = WebApplicationRule(
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
    Route.empty,
    Pages.empty,
    Record.empty
  )

  case class Pages(pages: List[Page]) {
    def get(view: View) = pages.find(_.isAccept(view))

    def complement(rhs: Pages): Pages = {
      case class Z(ms: Map[String, Page] = pages.map(x => (x.name, x)).toMap) {
        def r = Pages(ms.values.toList)
        def +(rhs: Page) = ms.get(rhs.name).
          map(x => copy(ms = ms + (x.name -> x.complement(rhs)))).
          getOrElse(copy(ms = ms + (rhs.name -> rhs)))
      }
      pages./:(Z())(_+_).r
    }
  }
  object Pages {
    val empty = Pages(Nil)

    def create(p: Option[List[Page]]) = p.fold(empty)(Pages(_))
  }

  case class Page(
    name: String,
    title: Option[I18NElement] = None,
    icon: Option[String] = None,
    contentHeaderStyle: Option[String] = None
  ) {
    def title(locale: Locale): NodeSeq = title.flatMap(_.get(locale)) getOrElse {
      XhtmlUtils.title(name)
    }

    def pathname: String = name // TODO

    def isAccept(p: View) = p match {
      case m: PageView => m.name == name
      case _ => false
    }

    def complement(rhs: Page): Page = Page(
      name,
      title orElse rhs.title,
      icon orElse rhs.contentHeaderStyle
    )
  }

  // TopMenu, SideBar
  case class FeatureList(
    page: List[Page],
    feature_list: Option[FeatureList]
  )

  // navigator
  case class UsecaseList(
    page: List[Page],
    usecase_list: Option[UsecaseList]
  )

  // navigator
  case class AdminList(
    page: List[Page],
    admin_list: Option[AdminList]
  )

  // footer
  case class InfoList(
    page: List[Page],
    info_list: Option[InfoList]
  )

  case class SinglePageApplication(
    base_uri: List[URI],
    rootPage: String = "index.html"
  ) {
    //    val pathname = base_uri.toString
    private val _pathnames = base_uri.map(_.toString)
    val rootPathname = _pathnames.head
    val rootPagePathname = StringUtils.concatPath(rootPathname, rootPage)
    def isActive(p: String): Boolean = _pathnames.exists(p.startsWith)
    def isRootPagePathname(p: String): Boolean = p == rootPagePathname
    def redirectCommand: MaterialCommand = MaterialCommand(rootPagePathname)
  }

  case class Http(
    cookieSecureKind: CookieSecureKind,
    loginMaxAge: FiniteDuration,
    accessMaxAge: FiniteDuration
  ) {
    def complement(rhs: Http) = this
  }
  object Http {
    val default = Http(SecureProductionCookieSecureKind, DEFAULT_LOGIN_MAXAGE, DEFAULT_ACCESS_MAXAGE)
  }

  sealed trait CookieSecureKind extends NamedValueInstance
  object SecureCookieSecureKind extends CookieSecureKind {
    val name = "secure"
  }
  object PublicCookieSecureKind extends CookieSecureKind {
    val name = "public"
  }
  object SecureProductionCookieSecureKind extends CookieSecureKind {
    val name = "secure-production"
  }

  object CookieSecureKind extends EnumerationClass[CookieSecureKind] {
    val elements = Vector(
      SecureCookieSecureKind,
      PublicCookieSecureKind,
      SecureProductionCookieSecureKind
    )
  }

  def parse(s: String): WebApplicationRule = WebApplicationConfig.parse(s).toRule
}
