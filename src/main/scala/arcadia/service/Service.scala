package arcadia.service

import arcadia._
import arcadia.context._

/*
 * @since   Mar. 20, 2022
 * @version Mar. 20, 2022
 * @author  ASAMI, Tomoharu
 */
trait Service {
  def invoke(op: InvokeOperationCommand): Response
}
