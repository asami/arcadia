package arcadia.standalone.service.generators

import com.typesafe.config.{Config => Hocon}
import org.goldenport.recorder.{ForwardRecorder, Recorder}
import org.goldenport.cli.Environment
import org.goldenport.io.InputSource
import arcadia.context.PlatformExecutionContext

/*
 * @since   Mar. 10, 2025
 * @version Mar. 13, 2025
 * @author  ASAMI, Tomoharu
 */
class Context(
  val environment: Environment,
  val config: Config,
  val platformExecutionContext: PlatformExecutionContext,
  val properties: Hocon,
  val library: List[InputSource]
) extends Environment.AppEnvironment with ForwardRecorder {
  protected def forward_Recorder: Recorder = recorder
  private def recorder = environment.recorder
}

object Context {
//  def create(): Context = create(Array[String]())

  def create(
    args: Array[String],
    pec: PlatformExecutionContext,
    props: Hocon
  ): Context = create(Environment.create(args), pec, props, Nil)

  def create(
    p: Environment,
    pec: PlatformExecutionContext,
    props: Hocon,
    lib: List[InputSource]
  ): Context = {
    new Context(p, Config(p.config, props), pec, props, lib)
  }
}
