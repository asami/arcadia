package arcadia.standalone.service

import org.goldenport.RAISE
import org.goldenport.i18n.I18NString
import org.goldenport.cli.{Config => CliConfig, _}
import org.goldenport.value._
import arcadia.standalone.service.operations._

/*
 * @since   Mar. 10, 2025
 * @version Mar. 11, 2025
 * @author  ASAMI, Tomoharu
 */
class ArcadiaService(
  config: Config,
  environment: Environment,
  services: Services,
  operations: Operations
) {
  private val _engine = Engine.standard(services, operations)

  def execute(args: Array[String]) = _engine.apply(environment, args)

  def run(args: Array[String]) {
    execute(args)
  }
}

object ArcadiaService {
  val PROP_STANDALONE_WEB_APPLICATION_NAME = "standalone"

  case object ArcadiaServiceClass extends ServiceClass {
    val name = "arcadia"
    val defaultOperation = None
    val operations = Operations(
      SiteOperationClass
    )
  }
}
