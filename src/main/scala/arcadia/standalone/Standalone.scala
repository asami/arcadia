package arcadia.standalone

import org.goldenport.RAISE
import org.goldenport.i18n.I18NString
import org.goldenport.cli.{Config => CliConfig, _}
import org.goldenport.value._
import arcadia.standalone.service._
import arcadia.standalone.service.ArcadiaService.ArcadiaServiceClass
import arcadia.standalone.service.operations._

/*
 * @since   Mar. 10, 2025
 * @version Mar. 12, 2025
 * @author  ASAMI, Tomoharu
 */
object Standalone {
  def main(args: Array[String]) {
    val env0 = Environment.create(args)
    val config = Config.create(env0)
    val context = new Context(env0, config)
    val env = env0.withAppEnvironment(context)
    val services = Services(
      ArcadiaServiceClass
    )
    val arcadia = new ArcadiaService(config, env, services, ArcadiaServiceClass.operations) // TODO
    arcadia.run(args)
  }
}
