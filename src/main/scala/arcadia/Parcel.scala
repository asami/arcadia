package arcadia

import java.net.URI
import org.goldenport.exception.RAISE
import org.goldenport.record.v2.Record
import arcadia.context._
import arcadia.domain._
import arcadia.model.Model
import arcadia.view.{RenderStrategy, Partials}

/*
 * @since   Jul. 15, 2017
 *  version Aug. 29, 2017
 * @version Sep. 21, 2017
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

  def getEffectiveModel: Option[Model] = model orElse command.flatMap(_.getModel)
  def toMessage: String = {
    command.map(_.toString) orElse
    getOperationName getOrElse ("Empty parcel")
  }
  def takeCommand[T <: Command]: T = command.map(_.asInstanceOf[T]) getOrElse {
    RAISE.noReachDefect
  }
  def getOperationName: Option[String] = context.flatMap(_.operationName)

  def goOrigin: Parcel = RAISE.notImplementedYetDefect
  def goError(e: Throwable): Parcel = RAISE.notImplementedYetDefect
  def goError(s: String): Parcel = RAISE.notImplementedYetDefect
  def goUnknownEvent(p: scenario.Event): Parcel = RAISE.notImplementedYetDefect

  def inputFormParameters: Record = context.map(_.inputFormParameters) getOrElse Record.empty
  def controllerUri: URI = context.map(_.controllerUri) getOrElse RAISE.noReachDefect
  def eventName: String = context.flatMap(_.getFormParameter("Submit")) getOrElse RAISE.notImplementedYetDefect
  def exception: Throwable = RAISE.notImplementedYetDefect
  def domainEntityType: DomainEntityType = context.flatMap(_.getFormParameter("web.entity.type")).map(DomainEntityType(_)) getOrElse RAISE.noReachDefect
  def domainEntityId: DomainObjectId = context.flatMap(_.getFormParameter("web.entity.id").map(StringDomainObjectId(_))) getOrElse RAISE.noReachDefect
}

object Parcel {
  def apply(model: Model, strategy: RenderStrategy): Parcel = Parcel(
    None, Some(model), Some(strategy), None
  )

  // def apply(command: Command, req: ServiceRequest): Parcel = Parcel(
  //   Some(command), command.getModel, None, None, Some(req)
  // )
  // def apply(command: Command, context: ExecutionContext): Parcel = Parcel(
  //   Some(command), command.getModel, None, Some(context), context.request
  // )
}
