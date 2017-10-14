package arcadia

import java.net.URI
import org.goldenport.exception.RAISE
import org.goldenport.record.v2.Record
import arcadia.context._
import arcadia.domain._
import arcadia.model.{Model, ErrorModel}
import arcadia.view.{ViewEngine, RenderStrategy, Partials, View}

/*
 * @since   Jul. 15, 2017
 *  version Aug. 29, 2017
 *  version Sep. 27, 2017
 * @version Oct. 14, 2017
 * @author  ASAMI, Tomoharu
 */
case class Parcel(
  command: Option[Command],
  model: Option[Model],
  view: Option[View],
  content: Option[Content],
  render: Option[RenderStrategy],
  platformContext: Option[PlatformExecutionContext],
  context: Option[ExecutionContext]
) {
  def withModel(model: Model) = copy(model = Some(model))
  def withView(view: View) = copy(view = Some(view))
  def withRenderStrategy(render: RenderStrategy) = copy(render = Some(render))

  // def withPartials(p: Partials) = render.fold(this)(r => copy(render = Some(r.copy(partials = p))))

  def withApplicationRule(p: WebApplicationRule) = copy(render = render.map(_.withApplicationRule(p)))
  def withApplicationConfig(p: WebApplicationConfig) = platformContext.
    map(x => copy(context = Some(ExecutionContext(x, p)))).
    getOrElse(RAISE.noReachDefect)

  def withContent(p: Content) = copy(content = Some(p))

  def forComponent(model: Model) = withModel(model).copy(command = None).componentScope
  def forView(engine: ViewEngine) =
    render.map(x => copy(render = Some(x.forView(engine, this)))) getOrElse {
      RAISE.noReachDefect
    }
  def sectionScope =
    render.map(x => copy(render = Some(x.withScopeSection))) getOrElse {
      RAISE.noReachDefect
    }
  def componentScope =
    render.map(x => copy(render = Some(x.withScopeContent))) getOrElse {
      RAISE.noReachDefect
    }

  def getEffectiveModel: Option[Model] = model orElse command.flatMap(_.getModel)
  def toMessage: String = {
    command.map(_.toString) orElse
    getOperationName getOrElse ("Empty parcel")
  }
  def takeCommand[T <: Command]: T = command.map(_.asInstanceOf[T]) getOrElse {
    RAISE.noReachDefect
  }
  def getOperationName: Option[String] = context.flatMap(_.getOperationName)

  def goOrigin: Parcel = RAISE.notImplementedYetDefect
  def goError(e: Throwable): Parcel = withModel(ErrorModel.create(this, e))
  def goError(s: String): Parcel = withModel(ErrorModel.create(this, s))
  def goUnknownEvent(p: scenario.Event): Parcel = withModel(ErrorModel.create(this, p))

  def inputFormParameters: Record = context.map(_.inputFormParameters) getOrElse Record.empty
  def controllerUri: URI = context.map(_.controllerUri) getOrElse RAISE.noReachDefect
//  def eventName: String = context.flatMap(_.getFormParameter("Submit")) getOrElse RAISE.notImplementedYetDefect
  def exception: Throwable = RAISE.notImplementedYetDefect
  def domainEntityType: DomainEntityType = context.flatMap(_.getFormParameter("web.entity.type")).map(DomainEntityType(_)) getOrElse RAISE.noReachDefect
  def domainEntityId: DomainObjectId = context.flatMap(_.getFormParameter("web.entity.id").map(StringDomainObjectId(_))) getOrElse RAISE.noReachDefect

  def execute[T](pf: ExecutionContext => T): T = executeOption(pf).getOrElse {
    RAISE.noReachDefect
  }

  def executeOption[T](pf: ExecutionContext => T): Option[T] = context.map(pf)

  def applyOnContext(pf: ExecutionContext => Parcel): Parcel = context.map(pf).getOrElse {
    RAISE.noReachDefect
  }
}

object Parcel {
  def apply(model: Model, strategy: RenderStrategy): Parcel = Parcel(
    None, Some(model), None, None, Some(strategy), None, None
  )

  // def apply(command: Command, req: ServiceRequest): Parcel = Parcel(
  //   Some(command), command.getModel, None, None, Some(req)
  // )
  // def apply(command: Command, context: ExecutionContext): Parcel = Parcel(
  //   Some(command), command.getModel, None, Some(context), context.request
  // )
}
