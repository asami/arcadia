package arcadia.service

import arcadia._
import arcadia.context._

/*
 * @since   Mar. 20, 2022
 * @version Mar. 20, 2022
 * @author  ASAMI, Tomoharu
 */
class SystemService(pc: PlatformContext) extends Service {
  def name = "system"

  def invoke(op: InvokeOperationCommand): Response = {
    op.request.operationName match {
      case "loopback" => _loopback(op)
      case _ => Response.notFound()
    }
  }

  private def _loopback(op: InvokeOperationCommand) = {
    val query = op.request.query
    val form = op.request.form
    Response.record(query + form)
  }
}
