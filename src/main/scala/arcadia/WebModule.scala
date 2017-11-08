package arcadia

import scalaz._, Scalaz._
import java.io.File
import java.net.URL
import org.fusesource.scalate._
import org.fusesource.scalate.support.URLTemplateSource
import com.asamioffice.goldenport.io.UURL
import org.goldenport.exception.RAISE
import org.goldenport.values.PathName
import org.goldenport.bag.ProjectVersionDirectoryBag
import org.goldenport.util.StringUtils
import arcadia.context._
import arcadia.controller._
import arcadia.view._

/*
 * @since   Jul. 23, 2017
 *  version Aug. 29, 2017
 *  version Sep. 17, 2017
 * @version Nov.  7, 2017
 * @author  ASAMI, Tomoharu
 */
abstract class WebModule() {
  import WebModule._
  def toWebApplication(platform: PlatformContext): WebApplication

  protected final def is_html(p: File): Boolean =
    is_html(StringUtils.toSuffix(p.getName))

  protected final def is_html(p: String): Boolean = p == "html"

  protected final def is_template(p: File): Boolean =
    is_template(p.getName)

  protected final def is_template(p: String): Boolean = {
    val suffix = StringUtils.toSuffix(p)
    templateSuffixes.contains(suffix)
  }
}

object WebModule {
  val templateSuffixes = Set("coffee", "md", "markdown", "ssp", "scaml", "mustache", "jade")
  val warSuffixes = Set("war", "zip")
  val htmlSuffixes = Set("html", "xhtml")

  def create(url: URL): WebModule = {
    val pathname = url.toString
    if (StringUtils.isSuffix(pathname, warSuffixes))
      new WarWebModule(url)
    else if (url.getProtocol == "file")
      DirectoryWebModule(url)
    else
      RAISE.invalidArgumentFault(s"$url")
  }
}

class DirectoryWebModule(base: File) extends WebModule {
  def toWebApplication(platform: PlatformContext) = {
    val builder = new WebApplication.Builder[File]() {
      protected def base_url: URL = to_url(base)
      protected def is_html(p: File): Boolean = DirectoryWebModule.this.is_html(p)
      protected def is_template(p: File): Boolean = DirectoryWebModule.this.is_template(p)
      protected def is_directory(p: File): Boolean = p.isDirectory
      protected def name(p: File): String = p.getName
      protected def to_url(p: File): URL = p.toURI.toURL
      protected def to_template_source(p: File): TemplateSource = TemplateSource.fromFile(p)
      protected def root_node: File = base
      protected def to_children(p: File): List[File] = p.listFiles.toList
    }
    builder.apply(platform)
  }
  // def toWebApplication = {
  //   val appname = "dashboard" // TODO
  //   case class Z(views: Vector[(Guard, View)] = Vector.empty) {
  //     def r = {
  //       val xs = views ++ Vector(
  //         AssetView.fromHtmlFilenameOrUri(base.getAbsolutePath()).gv
  //       )
  //       ViewEngine.Rule.create(xs)
  //     }
  //     def +(rhs: File) = {
  //       if (is_html(rhs))
  //         copy(views = views :+ HtmlView(rhs.toURI.toURL).gv)
  //       else if (is_template(rhs))
  //         copy(views = views :+ template_view(rhs))
  //       else if (rhs.isDirectory)
  //         copy(views = views ++ directory_view(rhs))
  //       else
  //         this
  //     }
  //     protected def template_view(p: File): (Guard, View) = {
  //       val name = StringUtils.pathLastComponentBody(p.getName)
  //       val src = TemplateSource.fromFile(p)
  //       name match {
  //         case "index" => IndexView(src).gv
  //         case "index.html" => IndexView(src).gv
  //         case "detail" => ResourceDetailView(src).gv
  //         case "detail.html" => ResourceDetailView(src).gv
  //         case "list" => ResourceListView(src).gv
  //         case "list.html" => ResourceListView(src).gv
  //         case "dashboard" => DashboardView(src).gv
  //         case "dashboard.html" => DashboardView(src).gv
  //         case m => PageView(src).gv
  //       }
  //     }
  //     protected def directory_view(p: File): Vector[(Guard, View)] = {
  //       Vector.empty // TODO
  //     }
  //   }
  //   val view = base.listFiles()./:(Z())(_+_).r
  //   val controller = WebApplication.standardControllerRule
  //   WebApplication(appname, controller, view)
  // }
}
object DirectoryWebModule {
  def fromPathname(dirname: String) = {
    new DirectoryWebModule(new File(dirname))
  }

  def apply(url: URL): DirectoryWebModule = fromPathname(url.getFile)
}

class WarWebModule(war: URL) extends WebModule {
  val basedir = {
    val r = new File("target/war")
    r.mkdirs()
    r
  }
  val bag = ProjectVersionDirectoryBag.createFromZip(basedir, war)
  lazy val module = new DirectoryWebModule(bag.homeDirectory)

  def toWebApplication(platform: PlatformContext) = module.toWebApplication(platform)
    // val builder = new WebApplication.Builder[File]() {
    //   protected def base_url: URL = to_url(base)
    //   protected def is_html(p: File): Boolean = DirectoryWebModule.this.is_html(p)
    //   protected def is_template(p: File): Boolean = DirectoryWebModule.this.is_template(p)
    //   protected def is_directory(p: File): Boolean = p.isDirectory
    //   protected def to_url(p: File): URL = p.toURI.toURL
    //   protected def to_template_source(p: File): TemplateSource = TemplateSource.fromFile(p)
    //   protected def root_children: List[File] = base.listFiles.toList
    //   protected def to_children(p: File): List[File] = p.listFiles.toList
    // }
    // builder.apply()
    // val appname = "dashboard" // TODO
    // case class Z() {
    //   def r: ViewEngine.Rule = ???
    //   def +(rhs: Tree[ZipBag.Node]) = _add_children(PathName("/"), rhs)
    //   def +(container: PathName, rhs: Tree[ZipBag.Node]) = rhs.rootLabel match {
    //     case ZipBag.RootNode => this
    //     case ZipBag.BagNode(name, bag) =>
    //       if (is_html(name))
    //         ???
    //       else if (is_template(name))
    //         ???
    //       else
    //         this
    //     case ZipBag.ContainerNode(name) => _add_children(container + name, rhs)
    //   }
    //   private def _add_children(container: PathName, p: Tree[ZipBag.Node]): Z =
    //     p.subForest./:(this)((z, x) => z + (container, x))
    // }
    // val view = war.assets.subForest./:(Z())(_+_).r
    // val controller = WebApplication.standardControllerRule
    // WebApplication(appname, controller, view)
}
object WarWebModule {
  def apply(uri: String): WarWebModule = {
    val url = UURL.getURLFromFileOrURLName(uri)
    new WarWebModule(url)
  }
}

class ResourceWebModule(resourcename: String, classloader: Option[ClassLoader]) extends WebModule {
  def toWebApplication(platform: PlatformContext) = RAISE.notImplementedYetDefect
}
