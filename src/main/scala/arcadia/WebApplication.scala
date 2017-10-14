package arcadia

import scalaz._, Scalaz._
import scala.xml.NodeSeq
import java.io.File
import java.net.URL
import java.util.Locale
import play.api.libs.json._
import scalax.io._
import org.fusesource.scalate._
import org.fusesource.scalate.support.URLTemplateSource
import org.goldenport.exception.RAISE
import org.goldenport.values.{Version, PathName}
import org.goldenport.bag.{ProjectVersionDirectoryBag, UrlBag}
import org.goldenport.util.StringUtils
import arcadia.context._
import arcadia.controller._
import arcadia.view._
import arcadia.view.ViewEngine._
import arcadia.view.tag.Tags
import arcadia.scenario._

/*
 * @since   Jul. 15, 2017
 *  version Aug. 29, 2017
 *  version Sep. 30, 2017
 * @version Oct. 14, 2017
 * @author  ASAMI, Tomoharu
 */
case class WebApplication(
  name: String,
  version: Option[Version],
  config: WebApplicationConfig,
  controller: ControllerEngine.Rule,
  view: ViewEngine.Rule
) {
  // def applicationTitle(locale: Locale): NodeSeq = config.applicationTitle(locale)
  // def applicationLogo(locale: Locale): NodeSeq = config.applicationLogo(locale)
  def extend: List[String] = config.extend getOrElse Nil
}

object WebApplication {
  val standardControllerRule = ControllerEngine.Rule.create(
    ResourceDetailController.gc,
    ResourceListController.gc,
    IndexController.gc,
    LoginController.gc,
    LogoutController.gc
  )

  lazy val empty = plain(PlatformContext.empty)

  val materialSuffixes = Set("html", "png", "jpg", "jpeg", "gif", "css", "js")

  def plain(platform: PlatformContext) = {
    val basedir = platform.getDevelopDirectory
    def source(name: String): TemplateSource = {
      val url = basedir.fold(getClass.getResource(name)) { x =>
        new File(x, name).toURI.toURL
      }
      new URLTemplateSource(url)
    }
    val entitydetailview = EntityDetailView(source("plain/detail.jade")).gv
      // val source = {
      //   if (true) {
      //     val filename = "/Users/asami/src/Project2017/EverforthFramework/src/main/resources/com/everforth/everforth/view/plain/detail.jade"
      //     TemplateSource.fromFile(filename)
      //   } else {
      //     val url = getClass.getResource("plain/detail.jade")
      //     new URLTemplateSource(url)
      //   }
      // }
    val entitylistview = EntityListView(source("plain/list.jade")).gv
      // val source = {
      //   if (true) {
      //     val filename = "/Users/asami/src/Project2017/EverforthFramework/src/main/resources/com/everforth/everforth/view/plain/list.jade"
      //     TemplateSource.fromFile(filename)
      //   } else {
      //     val url = getClass.getResource("plain/list.jade")
      //     new URLTemplateSource(url)
      //   }
      // }
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
    WebApplication("plain", None, config, ControllerEngine.Rule.empty, view)
  }

  // val bootstrap = {
  //   val resourcedetailview = {
  //     val source = {
  //       if (true) {
  //         val filename = "/Users/asami/src/Project2017/EverforthFramework/src/main/resources/com/everforth/everforth/view/bootstrap/detail.jade"
  //         TemplateSource.fromFile(filename)
  //       } else {
  //         val url = getClass.getResource("bootstrap/detail.jade")
  //         new URLTemplateSource(url)
  //       }
  //     }
  //     ResourceDetailView(source).gv
  //   }
  //   val resourcelistview = {
  //     val source = {
  //       if (true) {
  //         val filename = "/Users/asami/src/Project2017/EverforthFramework/src/main/resources/com/everforth/everforth/view/bootstrap/list.jade"
  //         TemplateSource.fromFile(filename)
  //       } else {
  //         val url = getClass.getResource("bootstrap/list.jade")
  //         new URLTemplateSource(url)
  //       }
  //     }
  //     ResourceListView(source).gv
  //   }
  //   val view = ViewEngine.Rule.create(
  //     resourcedetailview,
  //     resourcelistview
  //   )
  //   WebApplication("bootstrap", controller, view)
  // }

  // val foundation = {
  //   val resourcedetailview = {
  //     val source = {
  //       if (true) {
  //         val filename = "/Users/asami/src/Project2017/EverforthFramework/src/main/resources/com/everforth/everforth/view/foundation/detail.jade"
  //         TemplateSource.fromFile(filename)
  //       } else {
  //         val url = getClass.getResource("foundation/detail.jade")
  //         new URLTemplateSource(url)
  //       }
  //     }
  //     ResourceDetailView(source).gv
  //   }
  //   val resourcelistview = {
  //     val source = {
  //       if (true) {
  //         val filename = "/Users/asami/src/Project2017/EverforthFramework/src/main/resources/com/everforth/everforth/view/foundation/list.jade"
  //         TemplateSource.fromFile(filename)
  //       } else {
  //         val url = getClass.getResource("foundation/list.jade")
  //         new URLTemplateSource(url)
  //       }
  //     }
  //     ResourceListView(source).gv
  //   }
  //   val view = ViewEngine.Rule.create(
  //     resourcedetailview,
  //     resourcelistview
  //   )
  //   WebApplication("foundation", controller, view)
  // }

  // val dashboard = {
  //   val filename = "/tmp/bootstrap.d/paper-dashboard-free-v1.1.2/dashboard.html.ssp"
  //   val index = {
  //     val source = {
  //       TemplateSource.fromFile(new java.io.File(filename))
  //     }
  //     IndexView(source).gv
  //   }
  //   val view = ViewEngine.Rule.create(
  //     index,
  //     AssetView.fromHtmlFilenameOrUri(filename).gv
  //   )
  //   WebApplication("dashboard", controller, view)
  // }

  // val systemWebApplications = Vector(plain, bootstrap, foundation, dashboard)
  // lazy val dashboard = {
  //   val dirname = "/Users/asami/src/Project2017/admin/paper-dashboard-free-v1.1.2/"
  //   val module = DirectoryWebModule.fromPathname(dirname)
  //   module.toWebApplication
  // }

  // lazy val dashboardwar = {
  //   val filename = "/Users/asami/src/Project2017/admin/dashboard-1.0.war"
  //   val module = WarWebModule(filename)
  //   module.toWebApplication
  // }

//  lazy val systemWebApplications = Vector(plain, dashboard, dashboardwar)
  def systemWebApplications(platform: PlatformContext) = Vector(plain(platform))

  abstract class Builder[T]() {
    private lazy val (_application_name, _version) = ProjectVersionDirectoryBag.takeApplicationVersion(base_url)
    protected def applicationName: String = _application_name
    protected def version: Option[Version] = _version
    protected def base_url: URL
    protected def is_html(p: T): Boolean
    protected def is_template(p: T): Boolean
    protected def is_directory(p: T): Boolean
    // protected def is_material(p: T): Boolean =
    //   StringUtils.getSuffix(to_url(p).toString).
    //     fold(false)(x => materialSuffixes.contains(x.toLowerCase))
    protected def name(p: T): String
    protected def namebody(p: T): String = StringUtils.pathLastComponentBody(name(p))
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
        protected def directory_view(p: T): Vector[(Guard, View)] = {
          Vector.empty // TODO
        }
      }
      val config = build_config
      val view = {
        val applicationslots = root_children./:(Z())(_+_).r
        val layouts = build_layouts
        val partials = build_partials
        val comps = build_components
        val compslots = comps.toSlots
        val theme = config.theme.flatMap(RenderTheme.get)
        val slots = applicationslots ++ compslots
        val tags = Tags.empty // TODO
        ViewEngine.Rule.create(theme, slots, layouts, partials, comps, tags)
      }
      val controller = {
        val controllers = build_controllers
        ControllerEngine.Rule(controllers)
      }
      WebApplication(applicationName, version, config, controller, view)
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
            fold(this)(x => Z(r + (x -> LayoutView(to_template_source(rhs)))))
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
      case class Z(cs: Vector[ControllerEngine.Slot] = Vector.empty) {
        def r = cs
        def +(rhs: T) = {
          getNameSuffix(rhs).map {
            case "json" => copy(cs = cs :+ _json_controller(rhs))
            case _ => this
          }.getOrElse(this)
        }
      }
      get_pathnode(PathName("WEB-INF/controllers")).
        map(x => to_children(x)./:(Z())(_+_).r).getOrElse(Vector.empty)
    }

    private def _json_controller(rhs: T): ControllerEngine.Slot = {
      val name = namebody(rhs)
      val json = to_content_json(rhs)
      OperationController.get(name, json).map(x => ControllerEngine.Slot(x.gc)) getOrElse {
        RAISE.notImplementedYetDefect
      }
    }
  }
}
