package arcadia

import org.goldenport.exception.RAISE
import arcadia.context._
import arcadia.model.Model
import arcadia.view.{RenderStrategy, Partials}

/*
 * @since   Jul. 15, 2017
 *  version Aug. 29, 2017
 * @version Sep.  2, 2017
 * @author  ASAMI, Tomoharu
 */
case class Parcel(
  command: Option[Command],
  model: Option[Model],
  render: Option[RenderStrategy],
  context: Option[ExecutionContext]
) {
  def withModel(model: Model) = copy(model = Some(model))

  def withRenderStrategy(render: RenderStrategy) = copy(render = Some(render))

  // def withPartials(p: Partials) = render.fold(this)(r => copy(render = Some(r.copy(partials = p))))

  def withApplicationRule(p: WebApplicationRule) = copy(render = render.map(_.withApplicationRule(p)))

  def toMessage: String = {
    command.map(_.toString) orElse
    getOperationName getOrElse ("Empty parcel")
  }
  def takeCommand[T <: Command]: T = command.map(_.asInstanceOf[T]) getOrElse {
    RAISE.noReachDefect
  }
  def getOperationName: Option[String] = context.flatMap(_.operationName)
}

object Parcel {
  // def apply(command: Command, req: ServiceRequest): Parcel = Parcel(
  //   Some(command), command.getModel, None, None, Some(req)
  // )
  // def apply(command: Command, context: ExecutionContext): Parcel = Parcel(
  //   Some(command), command.getModel, None, Some(context), context.request
  // )
}
