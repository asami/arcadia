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
 * @version Aug. 30, 2017
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
  info_list: Option[WebApplicationRule.InfoList] // footer
) {
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
      info_list orElse rhs.info_list
    )
  }

  def complements(xs: Seq[WebApplicationRule]) = xs./:(this)(_ complement _)
}

object WebApplicationRule {
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
  }

  case class FeatureList(
    page: List[Page],
    feature_list: Option[FeatureList]
  )

  case class UsecaseList(
    page: List[Page],
    usecase_list: Option[UsecaseList]
  )

  case class AdminList(
    page: List[Page],
    admin_list: Option[AdminList]
  )

  case class InfoList(
    page: List[Page],
    info_list: Option[InfoList]
  )
}