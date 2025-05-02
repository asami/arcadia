package arcadia.standalone.service

import org.goldenport.cli.{Config => CliConfig, _}

/*
 * @since   Mar. 10, 2025
 * @version Mar. 10, 2025
 * @author  ASAMI, Tomoharu
 */
case class Context(
  env: Environment,
  config: Config
) extends Environment.AppEnvironment {
}
