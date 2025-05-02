package arcadia

import scalaz._, Scalaz._
// import java.io.File
import java.net.URL
import scala.util.control.NonFatal
import scala.collection.concurrent.TrieMap
import java.io.File
// import org.fusesource.scalate._
// import org.fusesource.scalate.support.URLTemplateSource
// import com.asamioffice.goldenport.io.UURL
import com.typesafe.config.{Config => Hocon}
import org.goldenport.RAISE
import org.goldenport.context._
import org.goldenport.io.{InputSource, FileInputSource, UrlInputSource}
import org.goldenport.realm.Realm
import org.goldenport.hocon.RichConfig.Implicits._
import org.goldenport.util.StringUtils
// import org.goldenport.exception.RAISE
// import org.goldenport.values.PathName
// import org.goldenport.bag.ProjectVersionDirectoryBag
// import org.goldenport.io.IoUtils
// import org.goldenport.util.StringUtils
import arcadia.context._
import arcadia.service.{ServiceFacility, SystemService}
import arcadia.view.TemplateEngineHangar
import arcadia.view.ScalateTemplateEngine
import arcadia.view.PugTemplateEngine
import arcadia.standalone.service.ArcadiaService.PROP_STANDALONE_WEB_APPLICATION_NAME
// import arcadia.controller._
// import arcadia.view._

/*
 * @since   Jan. 24, 2022
 *  version Feb. 28, 2022
 *  version Mar. 20, 2022
 *  version Jul. 24, 2022
 *  version Sep. 10, 2022
 *  version Oct. 23, 2022
 *  version Nov. 27, 2022
 *  version Dec. 25, 2022
 * @version Mar. 19, 2025
 * @author  ASAMI, Tomoharu
 */
class Arcadia(
  platformContext: PlatformContext,
  services: ServiceFacility,
  applications: Map[String, WebApplication],
  configs: Map[String, WebApplicationConfig],
  webEngineConfig: WebEngine.Config
) {
  private val _engines = new TrieMap[String, WebEngine]()

  def engine(name: String): WebEngine = _engine(name, Nil)

  private def _engine(name: String, history: List[String]): WebEngine =
    _engines.get(name) getOrElse {
      def h = if (history.isEmpty) "" else s""": ${history.mkString(",")}"""
      if (history.contains(name))
        Conclusion.config.illegalConfigurationDefect(s"Recursive '$name'$h").RAISE
      applications.get(name).fold {
        Conclusion.config.illegalConfigurationDefect(s"Invalid web application '$name'$h").RAISE
      } { app =>
        val extend = app.extend.map(_engine(_, name :: history))
        // TODO merge webRule
        val templateengines = {
          val a = webEngineConfig.templateEngineHangarFactory.create(platformContext)
          val b = if (webEngineConfig.useScalate)
            TemplateEngineHangar(new ScalateTemplateEngine(platformContext))
          else
            TemplateEngineHangar(new PugTemplateEngine(platformContext))
          a + b
        }
        val r = new WebEngine(
          platformContext,
          templateengines,
          services,
          app,
          extend,
          _web_config(app)
        )
        _engines += (name -> r)
        r
      }
    }

  private def _web_config(app: WebApplication) = configs.get(app.name) getOrElse WebApplicationConfig.empty

    def dispose(): Unit = synchronized {
      _engines.mapValues(_.shutdown())
      // fragile period in concurrency.
      _engines.clear()
    }
}

object Arcadia {
  val PROP_TMP_DIRECTORY = "tmp.directory"
  val PROP_WAR_URL = "war.url"
  val PROP_WAR_USER = "war.user"
  val PROP_WAR_PASSWORD = "war.password"

  // def create(pc: PlatformContext, appconfig: String): Arcadia = {
  // }

  // def create(pc: PlatformContext, configs: Map[String, WebApplicationRule]): Arcadia = {
  // }

  def make(
    pc: PlatformContext,
    webengineconfig: WebEngine.Config,
    config: Hocon,
    libs: Seq[InputSource],
    standalones: Seq[Realm]
  ): Consequence[Arcadia] = for {
    services <- _make_services(pc, webengineconfig, config)
    apps <- _make_applications(pc, webengineconfig, config, libs, standalones)
    confs <- _make_configs(config)
  } yield new Arcadia(pc, services, apps, confs, webengineconfig)

  private def _make_services(
    pc: PlatformContext,
    webengineconfig: WebEngine.Config,
    config: Hocon
  ): Consequence[ServiceFacility] = Consequence {
    val services = webengineconfig.services ++ Vector(new SystemService(pc))
    new ServiceFacility(pc, services.toVector)
  }

  private def _make_applications(
    pc: PlatformContext,
    webengineconfig: WebEngine.Config,
    config: Hocon,
    libs: Seq[InputSource],
    standalones: Seq[Realm]
  ): Consequence[Map[String, WebApplication]] = {
    val tmpdir = config.getFileOption(PROP_TMP_DIRECTORY) getOrElse new File("target/war")
    tmpdir.mkdirs

    def _make_application_(c: Hocon): Consequence[WebApplication] =
      for {
        url <- c.consequenceUrl(PROP_WAR_URL)
        user <- c.consequenceStringOption(PROP_WAR_USER)
        password <- c.consequenceStringOption(PROP_WAR_PASSWORD)
        r <- Consequence(
          WebModule.create(url, tmpdir, user, password).toWebApplication(pc, webengineconfig, config)
        )
      } yield r

    def _make_applications_in_webapps_(): Consequence[List[WebApplication]] = {
      val webapps = new File("webapps")
      Option(webapps.listFiles) match {
        case Some(s) => s.toList.traverse(_make_application_in_webapps_).map(_.flatten)
        case None => Consequence.success(Nil)
      }
    }

    def _make_application_in_webapps_(p: File): Consequence[List[WebApplication]] = {
      def _recover_app_(wm: WebModule, c: Conclusion): Consequence[WebApplication] =
        Consequence(WebApplication.error(wm.name, c))
      def _error_app_(p: File, c: Conclusion): Consequence[WebApplication] =
        Consequence(WebApplication.error(p.getName, c))

      Consequence(WebModule.createOption(p, tmpdir)) match {
        case Consequence.Success(s, _) =>
          s match {
            case Some(ss) => Consequence(List(ss.toWebApplication(pc, webengineconfig, config))).
                recoverWith {
                  case c => _recover_app_(ss, c).map(List(_))
                }
            case None => Consequence(Nil)
          }
        case Consequence.Error(c) => _error_app_(p, c).map(List(_))
      }
    }

    def _make_library_applications_(ps: Seq[InputSource]): Consequence[List[WebApplication]] =
      ps.toList.traverse(_make_library_application_)

    def _make_library_application_(p: InputSource): Consequence[WebApplication] = Consequence {
      val module = p match {
        case FileInputSource(file) => _make_module_file_(file)
        case UrlInputSource(url) => _make_module_url_(url)
        case m => RAISE.noReachDefect(s"_make_library_application_: $m")
      }
      module.toWebApplication(pc, webengineconfig, config)
    }

    def _make_module_file_(p: File) =
      WebModule.createOption(p, tmpdir) getOrElse {
        UnfoldResourceFault.parameter(p.toString).RAISE
      }

    def _make_module_url_(p: URL) =
      WebModule.create(p, tmpdir, None, None)

    def _make_standalone_applications_(ps: Seq[Realm]): Consequence[List[WebApplication]] =
      (ps.zipWithIndex).toList.traverse {
        case (realm, index) =>
          val name = if (index == 0)
            PROP_STANDALONE_WEB_APPLICATION_NAME
          else
            s"${PROP_STANDALONE_WEB_APPLICATION_NAME}${index}"
          val dir = new File(tmpdir, s"${name}.d")
          _make_standalone_application_(realm, dir, name)
      }.map(_.flatten)

    def _make_standalone_application_(
      p: Realm,
      tmpdir: File,
      name: String
    ): Consequence[Option[WebApplication]] = Consequence(
      WebModule.createOption(p, tmpdir).map(_.toWebApplication(pc, webengineconfig, config).withName(name))
    )

    // config.consequenceAsObjectList("application", _make_application)
    for {
      a <- config.consequenceAsObjectList("application", _make_application_)
      b <- _make_applications_in_webapps_()
      c <- _make_library_applications_(libs)
      d <- _make_standalone_applications_(standalones)
    } yield (a ++ b ++ c ++ d).map(x => x.name -> x).toMap
  }

//   private def _make_applications(pc: PlatformContext, config: Hocon, urls: Seq[URL]): Consequence[Map[String, WebApplication]] = Consequence {
//     val tmpdir = config.getFileOption(PROP_TMP_DIRECTORY) getOrElse new File("target/war")
//     tmpdir.mkdirs
//     val a = urls.flatMap { x =>
//       val user = config.getStringOption(PROP_WAR_USER)
//       val password = config.getStringOption(PROP_WAR_PASSWORD)
//       try {
// //        Some(WebModule.create(x, tmpdir, user, password).toWebApplication(pc))
//         Some(WebModule.create(x, tmpdir).toWebApplication(pc))
//       } catch {
//         case NonFatal(e) =>
//           None // TODO Consequence
//       }
//     }
//     val b = WebApplication.systemWebApplications(pc)
//     _select_application(a ++ b)
//   }

  private def _select_application(ps: Seq[WebApplication]): Map[String, WebApplication] = {
    case class Z(r: Map[String, WebApplication] = Map.empty) {
      def +(rhs: WebApplication) = {
        val name = rhs.basePath
        r.get(name).
          map(x => if (_is_high(rhs, x)) _add(name, rhs) else this).
          getOrElse(_add(name, rhs))
      }

      private def _is_high(l: WebApplication, r: WebApplication) =
        (l.version, r.version) match {
          case (Some(lv), Some(rv)) => lv.compare(rv) >= 0
          case (Some(lv), None) => false // No version means develop version
          case (None, Some(rv)) => true // No version means develop version
          case (None, None) => false // Definition order
        }

      private def _add(name: String, p: WebApplication) = copy(r = r + (name -> p))
    }
    ps./:(Z())(_+_).r
  }

  private def _make_configs(config: Hocon): Consequence[Map[String, WebApplicationConfig]] =
    Consequence(config.buildMap("configs", _get_config))

  private def _get_config(p: Hocon): Option[WebApplicationConfig] = ???
}
