package arcadia.service

import arcadia._
import arcadia.context._

/*
 * @since   Mar. 20, 2022
 * @version Mar. 21, 2022
 * @author  ASAMI, Tomoharu
 */
class ServiceFacility(
  platform: PlatformContext,
  services: Seq[Service]
) {
  def invoke(op: InvokeOperationCommand): Response = invokeOption(op) getOrElse Response.notFound()

  def invokeOption(op: InvokeOperationCommand): Option[Response] = services.toStream.flatMap { x =>
    val res = x.invoke(op)
    if (res.isNotFound)
      None
    else
      Some(res)
  }.headOption
}

object ServiceFacility {
}
