package arcadia.standalone.service.operations

import java.io.File
import com.typesafe.config.{Config => Hocon}
import org.goldenport.RAISE
import org.goldenport.context.Consequence
import org.goldenport.i18n.I18NString
import org.goldenport.cli._
import org.goldenport.io.InputSource
import org.goldenport.realm.Realm
import org.goldenport.util.StringUtils
import arcadia.context.PlatformExecutionContext
import arcadia.standalone.service.generators.{Context => ArcadiaContext}
import arcadia.standalone.service.generators.ArcadiaSiteGenerator

/*
 * @since   Mar. 10, 2025
 * @version Mar. 15, 2025
 * @author  ASAMI, Tomoharu
 */
case object SiteOperationClass extends OperationClassWithOperation {
  val request = SiteCommand.specification
  val response = SiteResult.specification
  val specification = spec.Operation("site", request, response)

  def apply(env: Environment, req: Request): Response = {
    val cmd = SiteCommand.create(req)
    val r = execute(env, cmd)
    FileRealmResponse(r.out)
  }

  def execute(env: Environment, cmd: SiteCommand): SiteResult = {
    val pce = PlatformExecutionContext.develop // TODO
    val config = cmd.config
    val realm = Realm.create(cmd.in)
    val libs = cmd.library
    val ctx = ArcadiaContext.create(env, pce, config, libs)
    val site = new ArcadiaSiteGenerator(ctx)
    val out = site.generate(libs, realm)
    SiteResult(out)
  }

  trait Command {
  }

  trait Result {
  }

  case class SiteCommand(
    in: File,
    config: Hocon,
    library: List[InputSource]
  ) extends Command {
  }
  object SiteCommand {
    object params {
      val in = spec.Parameter.argumentFile("in")
      val config = spec.Parameter.propertyConfigFileOrEmpty()
      val library = spec.Parameter.propertyInputSourceSequence("library")
    }

    def specification: spec.Request = spec.Request(
      params.in,
      params.config,
      params.library
    )

    def create(req: Request): SiteCommand =
      cCreate(req).take

    def cCreate(req: Request): Consequence[SiteCommand] = {
      for {
        in <- req.cFile(params.in)
        config <- req.cConfigOrZero(params.config)
        library <- req.cInputSourceList(params.library)
      } yield {
        SiteCommand(in, config, library)
      }
    }
  }

  case class SiteResult(
    out: Realm
  ) extends Result {
  }
  object SiteResult {
    def specification: spec.Response = spec.Response(spec.XRealm)
  }
}

