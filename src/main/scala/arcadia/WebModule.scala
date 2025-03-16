package arcadia

import scalaz._, Scalaz._
import java.io.File
import java.net.URL
import org.fusesource.scalate._
import org.fusesource.scalate.support.URLTemplateSource
import com.typesafe.config.{Config => Hocon}
import com.asamioffice.goldenport.io.UURL
import org.goldenport.exception.RAISE
import org.goldenport.values.PathName
import org.goldenport.bag.ProjectVersionDirectoryBag
import org.goldenport.realm.Realm
import org.goldenport.io.IoUtils
import org.goldenport.util.StringUtils
import arcadia.context._
import arcadia.controller._
import arcadia.view._
import arcadia.domain.DomainModel
import arcadia.domain.DomainModelSpace

/*
 * @since   Jul. 23, 2017
 *  version Aug. 29, 2017
 *  version Sep. 17, 2017
 *  version Nov. 10, 2017
 *  version Mar. 18, 2020
 *  version Jun.  2, 2020
 *  version Feb. 28, 2022
 *  version May. 22, 2022
 *  version Sep. 10, 2022
 *  version Nov. 27, 2022
 *  version Dec. 25, 2022
 *  version Jan.  1, 2023
 * @version Mar. 11, 2025
 * @author  ASAMI, Tomoharu
 */
abstract class WebModule() {
  import WebModule._
  def name: String

  def toWebApplication(
    platform: PlatformContext,
    webconfig: WebEngine.Config,
    config: Hocon
  ): WebApplication

  protected final def is_html(p: File): Boolean = isHtml(p.getName)
  // is_html(StringUtils.toSuffix(p.getName))

  protected final def is_html(p: String): Boolean = isHtml(p) //  == "html"

  protected final def is_template(p: File): Boolean =
    is_template(p.getName)

  protected final def is_template(p: String): Boolean = isTemplate(p)

  // protected final def is_template(p: String): Boolean = {
  //   val suffix = StringUtils.toSuffix(p)
  //   templateSuffixes.contains(suffix)
  // }
}

object WebModule {
  val templateSuffixes = Set("coffee", "md", "markdown", "ssp", "scaml", "mustache", "jade", "dox")
  val warSuffixes = Set("war", "zip")
  val htmlSuffixes = Set("html", "xhtml")

  def create(url: URL, basedir: File, user: Option[String], password: Option[String]): WebModule = {
    val pathname = url.toString
    if (StringUtils.isSuffix(pathname, warSuffixes))
      new WarWebModule(url, basedir, user, password)
    else if (url.getProtocol == "file")
      DirectoryWebModule(url)
    else
      RAISE.invalidArgumentFault(s"$url")
  }

  def createOption(file: File, basedir: File): Option[WebModule] = {
    val f = file.getCanonicalFile
    val name = f.getName
    if (StringUtils.isSuffix(name, warSuffixes))
      Some(new WarWebModule(file.toURI.toURL, basedir, None, None))
    else if (f.isDirectory)
      Some(DirectoryWebModule(f))
    else
      None
  }

  def createOption(realm: Realm, basedir: File): Option[WebModule] =
    Some(new RealmWebModule(realm, basedir))

  def isHtml(pathname: String): Boolean = {
    val suffix = StringUtils.toSuffix(pathname)
    htmlSuffixes.contains(suffix)
  }

  def isTemplate(pathname: String): Boolean = {
    val suffix = StringUtils.toSuffix(pathname)
    templateSuffixes.contains(suffix)
  }

  def toTemplateSource(p: File) = TemplateSource.fromFile(p)
}

class DirectoryWebModule(base: File) extends WebModule {
  def name = base.getName

  def toWebApplication(
    platform: PlatformContext,
    webconfig: WebEngine.Config,
    config: Hocon
  ) = {
    val builder = new WebApplication.Builder[File]() {
      protected def base_url: URL = to_url(base)
      protected def base_dir_for_dynamic_resolving = Some(base)
      protected def is_html(p: File): Boolean = DirectoryWebModule.this.is_html(p)
      protected def is_template(p: File): Boolean = DirectoryWebModule.this.is_template(p)
      protected def is_directory(p: File): Boolean = p.isDirectory
      protected def path(p: File): String = p.getPath
      protected def name(p: File): String = p.getName
      protected def to_url(p: File): URL = p.toURI.toURL
      protected def to_template_source(p: File): TemplateSource = TemplateSource.fromFile(p)
      protected def root_node: File = base
      protected def to_children(p: File): List[File] = p.listFiles.toList
      protected def to_descendants(p: File): List[File] = IoUtils.descendants(p).toList
      protected def parse_domain_model(p: File): Option[DomainModel] = webconfig.domainModelFactory.parse(p)
    }
    builder.apply(platform)
  }
}
object DirectoryWebModule {
  def fromPathname(dirname: String) = {
    new DirectoryWebModule(new File(dirname))
  }

  def apply(file: File): DirectoryWebModule = new DirectoryWebModule(file)
  def apply(url: URL): DirectoryWebModule = fromPathname(url.getFile)
}

class WarWebModule(
  war: URL,
  basedir: File,
  user: Option[String],
  password: Option[String]
) extends WebModule {
  // val basedir = {
  //   val r = new File("target/war")
  //   r.mkdirs()
  //   r
  // }
  val bag = ProjectVersionDirectoryBag.createFromZip(basedir, war, user, password)
  lazy val module = new DirectoryWebModule(bag.homeDirectory)

  def name = module.name

  def toWebApplication(
    platform: PlatformContext,
    webconfig: WebEngine.Config,
    config: Hocon
  ) = module.toWebApplication(platform, webconfig, config)
}
object WarWebModule {
  def apply(uri: String, basedir: File, user: Option[String], password: Option[String]): WarWebModule = {
    val url = UURL.getURLFromFileOrURLName(uri)
    new WarWebModule(url, basedir, user, password)
  }
}

class ResourceWebModule(resourcename: String, classloader: Option[ClassLoader]) extends WebModule {
  def name = resourcename

  def toWebApplication(
    platform: PlatformContext,
    webconfig: WebEngine.Config,
    config: Hocon
  ) = RAISE.notImplementedYetDefect
}

class RealmWebModule(
  realm: Realm,
  basedir: File
) extends WebModule {
  lazy val module = {
    realm.origin match {
      case Some(s) => 
        new DirectoryWebModule(s)
      case None => 
        realm.export(basedir)
        new DirectoryWebModule(basedir)
    }
  }

  def name = module.name

  def toWebApplication(
    platform: PlatformContext,
    webconfig: WebEngine.Config,
    config: Hocon
  ) = module.toWebApplication(platform, webconfig, config)
}
object RealmWebModule {
}
