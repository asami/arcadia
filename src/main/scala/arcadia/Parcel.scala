package arcadia

import scala.util.control.NonFatal
import java.net.URI
import org.goldenport.exception.RAISE
import org.goldenport.record.v2.Record
import org.goldenport.trace.{TraceContext, Result}
import org.goldenport.values.PathName
import org.goldenport.util.{SeqUtils, MapUtils}
import arcadia.context._
import arcadia.domain._
import arcadia.model.{Model, ErrorModel, Badge, IRecordModel}
import arcadia.view.{ViewEngine, RenderStrategy, Partials, View,
  UsageKind, TableKind, CardKind
}
import arcadia.controller.{Sink, ModelHangerSink, UrnSource}

/*
 * @since   Jul. 15, 2017
 *  version Aug. 29, 2017
 *  version Sep. 27, 2017
 *  version Oct. 31, 2017
 *  version Nov. 16, 2017
 *  version Jan. 15, 2018
 * @version Mar. 13, 2018
 * @author  ASAMI, Tomoharu
 */
case class Parcel(
  command: Option[Command],
  model: Option[Model],
  modelHanger: Map[String, Model],
  view: Option[View],
  content: Option[Content],
  render: Option[RenderStrategy],
  platformContext: Option[PlatformExecutionContext],
  context: Option[ExecutionContext],
  trace: Option[TraceContext]
) {
  def withCommand(p: Command) = copy(command = Some(p))
  def withModel(model: Model) = copy(model = Some(model))
  def withView(view: View) = copy(view = Some(view))
  def withContent(p: Content) = copy(content = Some(p))
  def withRenderStrategy(render: RenderStrategy) = copy(render = Some(render))

  // def withPartials(p: Partials) = render.fold(this)(r => copy(render = Some(r.copy(partials = p))))

  def withApplicationRule(p: WebApplicationRule) = copy(render = render.map(_.withApplicationRule(p)))
  def withApplication(p: WebApplication) = platformContext.
    map(x => copy(context = Some(ExecutionContext(x, p)))).
    getOrElse(RAISE.noReachDefect)
  def withTrace(p: TraceContext) = copy(trace = Some(p))

  def withUsageKind(p: UsageKind) = render.
    fold(RAISE.noReachDefect)(x => withRenderStrategy(x.withUsageKind(p)))
  def withTableKind(p: TableKind) = render.
    fold(RAISE.noReachDefect)(x => withRenderStrategy(x.withTableKind(p)))
  // def withCardKind(p: CardKind) = render.
  //   fold(RAISE.noReachDefect)(x => withRenderStrategy(x.withCardKind(p)))
  def withCardKindInGrid(p: CardKind) = render.
    fold(RAISE.noReachDefect)(x => withRenderStrategy(x.withCardKindInGrid(p)))

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

  // lazy val show: String = {
  //   val a = SeqUtils.buildTupleVector(
  //     "command" -> command.map(_.show),
  //     "model" -> model.map(_.show),
  //     "modelHanger" -> (if (modelHanger.isEmpty) None else Some(modelHanger.mapValues(_.show))),
  //     "view" -> view.map(_.show),
  //     "content" -> content.map(_.show),
  //     "strategy" -> render.map(_.show)
  //   )
  //   val b = a.map {
  //     case (k, v) => s"${k}=${v}"
  //   }.mkString(",")
  //   s"Parcel(${b})"
  // }

  lazy val show: String = {
    val a = Vector(
      command.map(_.show),
      model.map(_.show),
      (if (modelHanger.isEmpty) None else Some(MapUtils.show(modelHanger.mapValues(_.show)))),
      view.map(_.show),
      content.map(_.show),
      render.map(_.show)
    ).flatten
    val b = a.mkString(",")
    s"Parcel(${b})"
  }

  def toStrategy: RenderStrategy = render getOrElse {
    RAISE.noReachDefect
  }
  def getEffectiveModel: Option[Model] = model orElse command.flatMap(_.getModel)
  def getModel(key: String): Option[Model] = modelHanger.get(key)
  def toMessage: String = {
    command.map(_.toString) orElse
    getOperationName getOrElse ("Empty parcel")
  }
  def takeCommand[T <: Command]: T = command.map(_.asInstanceOf[T]) getOrElse {
    RAISE.noReachDefect
  }
  def getPathName: Option[PathName] = command.flatMap {
    case MaterialCommand(pathname) => Some(pathname)
    case _ => None
  }.orElse(context.flatMap(_.getPathName))
  def getOperationName: Option[String] =
    command flatMap {
      case MaterialCommand(pathname) => Some(pathname.body)
      case _ => context.flatMap(_.getOperationName)
    }
  def isOperationPathName(p: String): Boolean = {
    def ispathname(pathname: PathName) =
      p == pathname.body || pathname.getParent.fold(false)(_.body == p)
    command.flatMap {
      case MaterialCommand(pathname) => Some(ispathname(pathname))
      case _ => None
    }.getOrElse(
      context.
        flatMap(_.getOperationName.map(PathName(_))).
        map(ispathname).
        getOrElse(false)
    )
  }

  def getDomainObjectId: Option[DomainObjectId] = {
    val a = getEffectiveModel.flatMap {
      case m: IRecordModel => m.getDomainObjectId
      case _ => None
    }
    val b = command.flatMap(_.getDomainObjectId)
    a orElse b
  }

  def getLogicalUri: Option[URI] = context.flatMap(_.getLogicalUri)

  def resolvePathName(p: String): PathName = resolvePathName(PathName(p))
  def resolvePathName(p: PathName): PathName = context.fold(p)(_.resolvePathName(p))

  def getEntityType: Option[DomainEntityType] = render.flatMap(_.getEntityType)
  def fetchString(p: UrnSource): Option[String] = context.flatMap(_.fetchString(p))
  def fetchBadge(p: UrnSource): Option[Badge] = context.flatMap(_.fetchBadge(p))

  def goOrigin: Parcel = RAISE.notImplementedYetDefect
  def goError(e: Throwable): Parcel = withModel(ErrorModel.create(this, e))
  def goError(s: String): Parcel = withModel(ErrorModel.create(this, s))
  def goNotFound(s: String): Parcel = withModel(ErrorModel.notFound(this, s))
  def goUnknownEvent(p: scenario.Event): Parcel = withModel(ErrorModel.create(this, p))

  def inputQueryParameters: Record = context.map(_.inputQueryParameters) getOrElse Record.empty
  def inputFormParameters: Record = context.map(_.inputFormParameters) getOrElse Record.empty
  def controllerUri: URI = context.map(_.controllerUri) getOrElse RAISE.noReachDefect

  def webMeta: List[String] = inputQueryParameters.eagerStringList("_web")
  def isShowTrace: Boolean = webMeta.contains("show.trace")

//  def eventName: String = context.flatMap(_.getFormParameter("Submit")) getOrElse RAISE.notImplementedYetDefect
//  def exception: Throwable = RAISE.notImplementedYetDefect
//  def domainEntityType: DomainEntityType = context.flatMap(_.getFormParameter("web.entity.type")).map(DomainEntityType(_)) getOrElse RAISE.noReachDefect
//  def domainEntityId: DomainObjectId = context.flatMap(_.getFormParameter("web.entity.id").map(StringDomainObjectId(_))) getOrElse RAISE.noReachDefect

  def execute[T](pf: ExecutionContext => T): T = executeOption(pf).getOrElse {
    RAISE.noReachDefect
  }

  def executeOption[T](pf: ExecutionContext => T): Option[T] = context.map(pf)

  def applyOnContext(pf: ExecutionContext => Parcel): Parcel = context.map(pf).getOrElse {
    RAISE.noReachDefect
  }

  def executeWithTrace[T](label: String, entermessage: String)(body: => Result[T]): T = trace.
    map(_.execute(label, entermessage)(body)).
    getOrElse(try {
      body.r
    } catch {
      case NonFatal(e) => throw e
    })

  def executeWithTraceOption[T](label: String, entermessage: String)(body: => Option[Result[T]]): Option[T] = trace.
    map(_.executeOption(label, entermessage)(body)).
    getOrElse(try {
      body.map(_.r)
    } catch {
      case NonFatal(e) => throw e
    })

  def log(msg: String): Unit = trace.foreach(_.log(msg))

  def sink(s: Sink, m: Model): Parcel = s match {
    case ModelHangerSink(key) => copy(modelHanger = modelHanger ++ Map(key -> m))
  }
}

object Parcel {
  def apply(model: Model, strategy: RenderStrategy): Parcel = Parcel(
    None, Some(model), Map.empty, None, None, Some(strategy), None, None, None
  )

  // def apply(command: Command, req: ServiceRequest): Parcel = Parcel(
  //   Some(command), command.getModel, None, None, Some(req)
  // )
  // def apply(command: Command, context: ExecutionContext): Parcel = Parcel(
  //   Some(command), command.getModel, None, Some(context), context.request
  // )
}
