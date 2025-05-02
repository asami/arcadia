package arcadia.standalone.service.generators

import com.typesafe.config.{Config => Hocon}
import org.goldenport.RAISE
import org.goldenport.i18n.I18NString
import org.goldenport.cli.{Config => CliConfig, Environment}
import org.goldenport.value._

/*
 * @since   Mar. 10, 2025
 * @version Mar. 13, 2025
 * @author  ASAMI, Tomoharu
 */
case class Config(
  cliConfig: CliConfig,
  properties: Hocon
) {
}
