package arcadia.standalone.service

import org.goldenport.cli.{Config => CliConfig, _}

/*
 * @since   Mar. 10, 2025
 * @version Mar. 10, 2025
 * @author  ASAMI, Tomoharu
 */
case class Config() {
}

object Config {
  def create(env: Environment): Config = Config()
}
