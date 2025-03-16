package arcadia.standalone.service.generators

import org.goldenport.recorder.Recordable

/*
 * @since   Mar. 10, 2025
 * @version Mar. 10, 2025
 * @author  ASAMI, Tomoharu
 */
trait GeneratorBase extends Recordable {
  def context: Context

  set_Recorder(context)
}
