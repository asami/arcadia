package arcadia

import scalaz._, Scalaz._
import scala.xml.NodeSeq
import java.io.File
import java.net.URL
import java.util.Locale
import org.joda.time.{DateTime, DateTimeZone}
import play.api.libs.json._
import scalax.io._
import org.fusesource.scalate._
import org.fusesource.scalate.support.URLTemplateSource
import org.goldenport.exception.RAISE
import org.goldenport.context.Conclusion
import org.goldenport.context.FormatContext
import org.goldenport.values.{Version, PathName}
import org.goldenport.bag.{ProjectVersionDirectoryBag, UrlBag}
import org.goldenport.util.StringUtils
import arcadia.context._
import arcadia.controller._
import arcadia.view._
import arcadia.view.ViewEngine._
import arcadia.view.tag.Tags
import arcadia.scenario._
// import arcadia.domain.DomainModelEngine
import arcadia.domain.DomainModel
import arcadia.domain.DomainModelSpace

/*
 * @since   Jul. 15, 2017
 *  version Aug. 29, 2017
 *  version Sep. 30, 2017
 *  version Oct. 27, 2017
 *  version Nov.  6, 2017
 *  version Jan.  6, 2018
 *  version Apr. 28, 2019
 *  version Mar. 18, 2020
 *  version Apr. 13, 2020
 *  version Apr. 15, 2021
 *  version May. 22, 2022
 *  version Jul. 25, 2022
 *  version Nov. 27, 2022
 *  version Dec. 29, 2022
 * @version Jan.  1, 2023
 * @author  ASAMI, Tomoharu
 */
case class WebApplication(
  name: String,
  version: Option[Version],
  config: WebApplicationConfig,
  controller: ControllerEngine.Rule,
  view: ViewEngine.Rule,
  domain: DomainModel
) {
  def getLocale = config.getLocale
  def extend: List[String] = config.extend getOrElse Nil
  def basePath: String = config.base_path getOrElse name
  // def format(locale: Locale, tz: DateTimeZone, p: DateTime): Option[String] = None
  // def formatDateTime(locale: Locale, tz: DateTimeZone, p: DateTime): Option[String] = None
  // def formatDate(locale: Locale, tz: DateTimeZone, p: DateTime): Option[String] = None
  // def formatTime(locale: Locale, tz: DateTimeZone, p: DateTime): Option[String] = None
  def getFormatContext: Option[FormatContext] = None
}

object WebApplication {
  val standardControllerRule = ControllerEngine.Rule.create(
    DomainModelController.gc,
    ResourceDetailController.gc, // unused
    ResourceListController.gc, // unused
    IndexController.gc,
    LoginController.gc,
    LogoutController.gc
  )

  lazy val empty = plain(PlatformContext.empty)

  val materialSuffixes = Set("html", "png", "jpg", "jpeg", "gif", "apng", "webp", "avif", "css", "js")

  def plain(platform: PlatformContext) = {
    val basedir = platform.getDevelopDirectory
    def source(name: String): TemplateSource = {
      val url = basedir.fold(getClass.getResource(name)) { x =>
        new File(x, name).toURI.toURL
      }
      new URLTemplateSource(url)
    }
    val entitydetailview = EntityDetailView(source("plain/detail.jade")).gv
    val entitylistview = EntityListView(source("plain/list.jade")).gv
    val dashboardview = DashboardView(source("plain/dashboard.jade")).gv
    val modelview = ModelView(source("plain/model.jade")).gv
    val view = ViewEngine.Rule.create(
      entitydetailview,
      entitylistview, // TODO component
      dashboardview, // TODO component
      modelview
    )
    val scenario = ScenarioEngine.Rule.empty
    val config = WebApplicationConfig.create("Plain")
    WebApplication("plain", None, config, ControllerEngine.Rule.empty, view, DomainModel.empty)
  }

  def error(name: String, c: Conclusion) = {
    val config = WebApplicationConfig.empty
    val controller = ControllerEngine.Rule.empty
    val view = ViewEngine.Rule.error
    WebApplication(name, None, config, controller, view, DomainModel.empty)
  }

//  lazy val systemWebApplications = Vector(plain, dashboard, dashboardwar)
  def systemWebApplications(platform: PlatformContext) = Vector(plain(platform))

  abstract class Builder[T]() {
    private lazy val (_application_name, _version) = ProjectVersionDirectoryBag.takeApplicationVersion(base_url)
    protected def application_name_by_url: String = _application_name
    protected def version: Option[Version] = _version
    protected def base_url: URL
    protected def base_dir_for_dynamic_resolving: Option[File]
    protected def is_html(p: T): Boolean
    protected def is_template(p: T): Boolean
    protected def is_directory(p: T): Boolean
    // protected def is_material(p: T): Boolean =
    //   StringUtils.getSuffix(to_url(p).toString).
    //     fold(false)(x => materialSuffixes.contains(x.toLowerCase))
    protected def is_web_info(p: T): Boolean = name(p) == "WEB-INF"
    protected def is_assets(p: T): Boolean = name(p) == "assets"
    protected def path(p: T): String
    protected def name(p: T): String
    protected def namebody(p: T): String = StringUtils.pathLastComponentBody(name(p))
    protected def relativePathBody(root: T, p: T): String = StringUtils.pathRelativeBody(path(root), path(p))
    protected def getNameSuffix(p: T): Option[String] = StringUtils.getSuffix(name(p))
    protected def to_url(p: T): URL
    protected def to_template_source(p: T): TemplateSource
    protected def to_content_string(p: T): String = Resource.fromURL(to_url(p)).string
    protected def to_content_json(p: T): JsValue = Json.parse(to_content_string(p))
    protected def get_pathnode(path: PathName): Option[T] =
      get_pathnode(root_node, path)
    protected def get_pathnode(p: T, path: PathName): Option[T] = {
      assert (!path.isAbsolute, s"No relative path: $path")
      def go(n: T, p: List[String]): Option[T] = p match {
        case Nil => Some(n)
        case x :: Nil => get_node(n, x)
        case x :: xs => get_node(n, x).flatMap(n => go(n, xs))
      }
      go(root_node, path.components)
    }
    protected def get_node(p: T, s: String): Option[T] = to_children(p).find(x => name(x) === s)
    protected def root_node: T
    protected def root_children: List[T] = to_children(root_node)
    protected def to_children(p: T): List[T]
    protected def to_children(path: PathName): List[T] = {
      get_pathnode(root_node, path).map(to_children).orZero
    }
    protected def to_descendants(p: T): List[T]
    protected def parse_domain_model(p: T): Option[DomainModel]

    def apply(platform: PlatformContext): WebApplication = {
      case class Z(views: Vector[(Guard, View)] = Vector.empty) {
        def r: Seq[Slot] = views.map(Slot(_)) ++ Vector(
          Slot(AssetView(base_url).gv),
          Slot(MaterialView(base_url).gv)
        )
        def +(rhs: T) = {
          if (is_html(rhs))
            copy(views = views :+ HtmlView(to_url(rhs)).gv)
          else if (is_template(rhs))
            copy(views = views :+ template_view(rhs))
          else if (is_directory(rhs))
            copy(views = views ++ directory_view(rhs))
          // else if (is_material(rhs))
          //   copy(views = views :+ MaterialView(to_url(rhs)).gv)
          else
            this
        }
        protected def template_view(p: T): (Guard, View) = {
          val name = StringUtils.pathLastComponentBody(to_url(p).toString)
          val src = to_template_source(p)
          name match {
            case "index" => IndexView(src).gv
//            case "detail" => ResourceDetailView(src).gv 
//            case "list" => ResourceListView(src).gv
            case "dashboard" => DashboardView(src).gv
            case m => PageView(m, src).gv
          }
        }
        protected def directory_view(p: T): Vector[(Guard, View)] =
          if (is_web_info(p) || is_assets(p))
            Vector.empty
          else
            to_children(p)./:(ZZ(name(p)))(_+_).views
      }
      case class ZZ(
        base: String,
        views: Vector[(Guard, View)] = Vector.empty
      ) {
        def r: Seq[Slot] = views.map(Slot(_))
        def +(rhs: T) = {
          if (is_html(rhs))
            copy(views = views :+ html_view(rhs))
          else if (is_template(rhs))
            copy(views = views :+ template_view(rhs))
          else if (is_directory(rhs))
            copy(views = views ++ directory_view(rhs))
          else
            this
        }
        protected def html_view(p: T): (Guard, View) =
          HtmlView(to_url(p)).gv
        protected def template_view(p: T): (Guard, View) = {
          val src = to_template_source(p)
          PageView(pathname(p), src).gv
        }
        protected def directory_view(p: T): Vector[(Guard, View)] =
          to_children(p)./:(ZZ(pathname(p)))(_+_).views

        protected def pathname(p: T): String = 
          s"$base/${StringUtils.pathLastComponentBody(to_url(p).toString)}"
      }
      val config = build_config
      val view = {
        val applicationslots = root_children./:(Z())(_+_).r
        val layouts = build_layouts
        val partials = build_partials
        val pages = build_pages
        val comps = build_components
        val compslots = comps.toSlots
        val theme = config.theme.flatMap(RenderTheme.get)
        val slots = applicationslots ++ compslots
        val tags = Tags.empty // TODO
        val spa = config.singlePageApplication.map(_.toRule)
        ViewEngine.Rule.create(theme, slots, layouts, partials, pages, comps, tags, spa, base_dir_for_dynamic_resolving)
      }
      val controller = {
        val controllers = build_controllers
        ControllerEngine.Rule(controllers)
      }
      val applicationname = config.name getOrElse application_name_by_url
      val domainmodel = build_domain_model
      WebApplication(applicationname, version, config, controller, view, domainmodel)
    }

    private def _is_view(s: String, t: T): Boolean =
      s === namebody(t) && _is_view(t)

    private def _is_view(t: T): Boolean = getNameSuffix(t).fold(false)(suffix =>
      WebModule.templateSuffixes.contains(suffix) ||
        WebModule.htmlSuffixes.contains(suffix)
    )

    protected def build_config: WebApplicationConfig = {
      val a = _get_rule("WEB-INF/webapp.conf")
      val b = _get_rule("WEB-INF/webapp.json")
      (a, b) match {
        case (Some(l), Some(r)) => l complement r
        case (Some(l), None) => l
        case (None, Some(r)) => r
        case (None, None) => WebApplicationConfig.empty
      }
    }

    private def _get_rule(p: String): Option[WebApplicationConfig] =
      for {
        node <- get_pathnode(PathName(p))
      } yield {
        val url = to_url(node)
        val bag = new UrlBag(url)
        WebApplicationConfig.parse(bag.toText)
      }

    protected def build_layouts: Map[LayoutKind, LayoutView] = {
      case class Z(r: Map[LayoutKind, LayoutView] = Map.empty) {
        def +(rhs: T) = {
          LayoutKind.elements.find(x => _is_view(x.name, rhs)).
            map(x =>
              Z(r + (x -> LayoutView(to_template_source(rhs))))
            ).getOrElse(
              if (_is_view(rhs))
                Z(r + (PageLayout(namebody(rhs)) -> LayoutView(to_template_source(rhs))))
              else
                this
            )
        }
      }
      get_pathnode(PathName("WEB-INF/layouts")).
        map(x => to_children(x)./:(Z())(_+_).r).getOrElse(Map.empty)
    }

    protected def build_partials: Partials = {
      case class Z(m: Map[PartialKind, PartialView] = Map.empty) {
        val r = Partials(m)
        def +(rhs: T) = {
          PartialKind.elements.find(x => _is_view(x.name, rhs)).
            fold(this)(x => Z(m + (x -> PartialView(to_template_source(rhs)))))
        }
      }
      get_pathnode(PathName("WEB-INF/partials")).
        map(x => to_children(x)./:(Z())(_+_).r).getOrElse(Partials.empty)
    }

    protected def build_pages: Pages = {
      case class Z(views: Vector[(PathName, View)] = Vector.empty) {
        val r = Pages(views.toMap)
        def +(rhs: T) = {
          if (is_html(rhs))
            copy(views = views :+ (PathName.home -> HtmlView(to_url(rhs))))
          else if (is_template(rhs))
            copy(views = views :+ (PathName.home -> templateview(rhs)))
          else if (is_directory(rhs))
            copy(views = views ++ directoryview(rhs))
          else
            this
        }

        private def templateview(p: T): View = {
          val src = to_template_source(p)
          val name = StringUtils.pathLastComponentBody(to_url(p).toString)
          PageView(name, src)
        }

        private def directoryview(p: T): Vector[(PathName, View)] = {
          val name = StringUtils.pathLastComponentBody(to_url(p).toString) // TODO
          to_children(p)./:(ZZ(PathName(name), name))(_+_).r
        }
      }
      case class ZZ(
        pathname: PathName,
        base: String,
        views: Vector[(PathName, View)] = Vector.empty
      ) {
        def r = views
        def +(rhs: T) = {
          val n = StringUtils.pathLastComponentBody(to_url(rhs).toString)
          def pn: PathName = pathname + n
          if (is_html(rhs))
            copy(views = views :+ (pn -> htmlview(rhs)))
          else if (is_template(rhs))
            copy(views = views :+ (pn -> templateview(rhs)))
          else if (is_directory(rhs))
            copy(views = views ++ directoryview(pn, rhs))
          else
            this
        }

        protected def htmlview(p: T): View =
          HtmlView(to_url(p))
        protected def templateview(p: T): View = {
          val src = to_template_source(p)
          PageView(toname(p), src)
        }
        protected def directoryview(pn: PathName, p: T): Vector[(PathName, View)] =
          to_children(p)./:(ZZ(pn, toname(p)))(_+_).r

        protected def toname(p: T): String = 
          s"base/${StringUtils.pathLastComponentBody(to_url(p).toString)}"
      }
      get_pathnode(PathName("WEB-INF/pages")).
        map(x => to_children(x)./:(Z())(_+_).r).getOrElse(Pages.empty)
    }

    protected def build_components: Components = {
      case class Z(m: Vector[ComponentView] = Vector.empty) {
        val r = Components(m)
        def +(rhs: T) = {
          if (_is_view(rhs))
            Z(m :+ ComponentView.create(namebody(rhs), to_template_source(rhs)))
          else
            this
        }
      }
      get_pathnode(PathName("WEB-INF/components")).
        map(x => to_children(x)./:(Z())(_+_).r).getOrElse(Components.empty)
    }

    protected def build_controllers: Vector[ControllerEngine.Slot] = {
      case class Z(
        root: T,
        cs: Vector[ControllerEngine.Slot] = Vector.empty
      ) {
        def r = cs
        def +(rhs: T) = {
          getNameSuffix(rhs).map {
            case "json" => copy(cs = cs :+ _json_controller(root, rhs))
            case _ => this
          }.getOrElse(this)
        }
      }
      get_pathnode(PathName("WEB-INF/controllers")).
        map(x => to_descendants(x)./:(Z(x))(_+_).r).getOrElse(Vector.empty)
    }

    private def _json_controller(root: T, rhs: T): ControllerEngine.Slot = {
      val name = relativePathBody(root, rhs)
      val json = to_content_json(rhs)
      OperationController.get(name, json).map(x => ControllerEngine.Slot(x.gc)) getOrElse {
        RAISE.notImplementedYetDefect
      }
    }

    protected def build_domain_model: DomainModel = {
      case class Z(
        model: DomainModel = DomainModel.empty
      ) {
        def r = model
        def +(rhs: T) =
          parse_domain_model(rhs).map(x => copy(model = model add x)).getOrElse(this)
      }
      get_pathnode(PathName("WEB-INF/models")).
        map(x => to_descendants(x)./:(Z())(_+_).r).getOrElse(DomainModel.empty)
    }
  }

  // case class ErrorBuilder() extends Builder[Unit]() {
  //   protected def base_url: URL = ???
  //   protected def base_dir_for_dynamic_resolving: Option[File] = None
  //   protected def is_html(p: Unit): Boolean = ???
  //   protected def is_template(p: Unit): Boolean = ???
  //   protected def is_directory(p: Unit): Boolean = ???
  //   protected def path(p: Unit): String = ???
  //   protected def name(p: Unit): String = ???
  //   protected def to_url(p: Unit): URL = ???
  //   protected def to_template_source(p: Unit): TemplateSource = ???
  //   protected def root_node: Unit = ???
  //   protected def to_children(p: Unit): List[Unit] = ???
  //   protected def to_descendants(p: Unit): List[Unit] = ???
  // }
}
