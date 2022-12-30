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
import org.goldenport.context.Consequence
import org.goldenport.context.Conclusion
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
 * @version Dec. 25, 2022
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
          val b = TemplateEngineHangar(new ScalateTemplateEngine(platformContext))
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
    config: Hocon
  ): Consequence[Arcadia] = for {
    services <- _make_services(pc, webengineconfig, config)
    apps <- _make_applications(pc, webengineconfig, config)
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
    config: Hocon
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
      webapps.listFiles.toList.traverse(_make_application_in_webapps_).map(_.flatten)
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

    // config.consequenceAsObjectList("application", _make_application)
    for {
      a <- config.consequenceAsObjectList("application", _make_application_)
      b <- _make_applications_in_webapps_()
    } yield (a ++ b).map(x => x.name -> x).toMap
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
