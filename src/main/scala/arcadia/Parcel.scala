package arcadia

import scala.util.control.NonFatal
import java.util.Locale
import java.net.URI
import play.api.libs.json._
import org.goldenport.exception.RAISE
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.record.v2.Invalid
import org.goldenport.trace.{TraceContext, Result}
import org.goldenport.values.PathName
import org.goldenport.util.{SeqUtils, MapUtils, StringUtils}
import arcadia.context._
import arcadia.service.ServiceFacility
import arcadia.domain._
import arcadia.model.{Model, ErrorModel, Badge, IRecordModel, CandidatesModel}
import arcadia.model.PropertySheetModel
import arcadia.model.Property
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
 *  version Mar. 26, 2018
 *  version Jul. 17, 2018
 *  version Aug. 31, 2018
 *  version Sep.  5, 2018
 *  version Apr. 30, 2019
 *  version May.  1, 2019
 *  version Mar. 31, 2020
 *  version Apr. 20, 2020
 *  version Jun.  3, 2020
 *  version Mar. 21, 2022
 *  version May.  3, 2022
 *  version Dec. 29, 2022
 * @version Mar. 31, 2023
 * @author  ASAMI, Tomoharu
 */
case class Parcel(
  command: Option[Command],
  model: Option[Model],
  modelHanger: Map[String, Model],
  propertyModel: Option[PropertySheetModel],
  view: Option[View],
  content: Option[Content],
  render: Option[RenderStrategy],
  session: Option[Session],
  platformExecutionContextOption: Option[PlatformExecutionContext],
  context: Option[ExecutionContext],
  trace: Option[TraceContext]
) {
  def getPlatformExecutionContext: Option[PlatformExecutionContext] =
    platformExecutionContextOption orElse context.map(_.platformExecutionContext)
  def getPlatformContext: Option[PlatformContext] = getPlatformExecutionContext.map(_.platformContext)

  def withCommand(p: Command) = copy(command = Some(p))
  def withModel(model: Model) = copy(model = Some(model))
  def withCommandModel(c: Command, m: Model) = copy(command = Some(c), model = Some(m))
  def withViewModel(v: String, m: Model) = copy(command = Some(ViewCommand(v)), model = Some(m))
  def withView(view: View) = copy(view = Some(view))
  def withContent(p: Content) = copy(content = Some(p))
  def withRenderStrategy(render: RenderStrategy) = copy(render = Some(render))

  // def withPartials(p: Partials) = render.fold(this)(r => copy(render = Some(r.copy(partials = p))))

  // def withApplicationRule(p: WebApplicationRule) = copy(render = render.map(_.withApplicationRule(p)))
  def complementApplicationRule(p: WebApplicationRule) = copy(render = render.map(_.complementApplicationRule(p)))
  def withExecutionContext(service: ServiceFacility, app: WebApplication) = getPlatformExecutionContext.
    map(x => copy(context = Some(ExecutionContext(x, service, app)))).
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

  def withSession(p: Session) = copy(session = Some(p))

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

  def setRedirect(p: String): Parcel = {
    val pn = command.collect {
      case MaterialCommand(pathname) => pathname.components.length match {
        case 0 => p
        case 1 => p
        case n => StringUtils.concatPath(List.fill(n - 1)("..").mkString("/"), p)
      }
    }.getOrElse(p)
    withContent(RedirectContent(pn, session))
  }

  // Lazy val show: String = {
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

  def addProperties(p: List[Property]) = {
    val a = propertyModel.map(x => Record.create(x.record)).getOrElse(Record.empty)
    val b = p./:(a)((z, x) => z.update(x.name, _value(x)))
    val c = if (b.isEmpty)
      None
    else
      Some(PropertySheetModel(b))
    copy(propertyModel = c)
  }

  private def _value(p: Property) = p.value match {
    case JsNumber(v) => v
    case JsString(s) => s
    case _ => ???
  }

  lazy val show: String = try {
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
  } catch {
    case NonFatal(e) => s"Parcel(${e})"
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
  def pathUri: URI = new URI(getPathName.map(_.v) getOrElse RAISE.noReachDefect) // XXX
  def getPathName: Option[PathName] = command.flatMap {
    case MaterialCommand(pathname) => Some(pathname)
    case m: IndexCommand => Some(m.pathname)
    case ViewCommand(pathname) => Some(pathname)
    case _ => None
  }.orElse(context.flatMap(_.getPathName))
  def getOperationName: Option[String] =
    command flatMap {
      case MaterialCommand(pathname) => Some(pathname.body)
      case m: IndexCommand => Some(m.body)
      case ViewCommand(pathname) => Some(pathname.body)
      case _ => context.flatMap(_.getOperationName)
    }
  def isOperationPathName(p: String): Boolean = {
    val basename = PathName(p)
    // def ispathname(pathname: PathName) =
    //   p == pathname.body || pathname.getParent.fold(false)(_.body == p)
    command.flatMap {
      case MaterialCommand(pathname) => Some(_is_operation_pathname(basename, pathname))
      case m: IndexCommand => Some(_is_operation_pathname(basename, m.pathname))
      case ViewCommand(pathname) => Some(_is_operation_pathname(basename, pathname))
      case _ => None
    }.getOrElse(
      context.
        flatMap(_.getOperationName.map(PathName(_))).
        map(_is_operation_pathname(basename, _)).
        getOrElse(false)
    )
  }

  private def _is_operation_pathname(basename: PathName, pn: PathName) = {
    case class Z(bs: List[String], matchp: Boolean = true) {
      def r = matchp && bs.isEmpty
      def +(rhs: String) =
        if (matchp) {
          bs match {
            case x :: xs =>
              if (StringUtils.toPathnameBody(x) == StringUtils.toPathnameBody(rhs))
                Z(xs, true)
              else
                Z(xs, false)
            case Nil => this
          }
        } else {
          this
        }
    }
    pn.components./:(Z(basename.components))(_+_).r
  }

  def getPathDepthForRedirect: Option[Int] = getPathName.map { x =>
    val cs = x.components
    if (x.v.endsWith("/"))
      cs.length
    else
      cs.length - 1
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
  def fetchCandidates(p: String): CandidatesModel = getModel(p).collect {
    case m: CandidatesModel => m
  }.getOrElse {
    context.flatMap(_.fetchCandidates(p)).getOrElse(RAISE.noReachDefect)
  }
  def goOrigin: Parcel = RAISE.notImplementedYetDefect
  def goError(e: Throwable): Parcel = goError(ErrorModel.create(this, e))
  def goError(p: Invalid): Parcel = goError(ErrorModel.create(this, p))
  def goError(s: String): Parcel = goError(ErrorModel.create(this, s))
  def goError(m: ErrorModel): Parcel = withCommandModel(ErrorCommand(m), m)
  def goNotFound(s: String): Parcel = goError(ErrorModel.notFound(this, s))
  def goUnknownEvent(p: scenario.Event): Parcel = goError(ErrorModel.create(this, p))

  def inputQueryParameters: IRecord = context.map(_.inputQueryParameters) getOrElse Record.empty
  def inputFormParameters: IRecord = context.map(_.inputFormParameters) getOrElse Record.empty
  def inputQueryFormParameters: IRecord = inputQueryParameters + inputFormParameters
  def controllerUri: URI = context.map(_.controllerUri) getOrElse RAISE.noReachDefect

  def webMeta: List[String] = inputQueryParameters.takeStringList("_web")
  def isShowTrace: Boolean = webMeta.contains("show.trace")

  def locale: Locale = context.map(_.locale) orElse getPlatformExecutionContext.map(_.locale) getOrElse Locale.US

  def getDomainModel: Option[DomainModel] = context.map(_.webapp.domain)

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

  def sink(s: Sink, command: Command, m: Model): Parcel = s match {
    case ModelHangerSink(key) =>
      copy(command = Some(command), modelHanger = modelHanger ++ Map(key -> m))
  }
}

object Parcel {
  def apply(model: Model, strategy: RenderStrategy): Parcel = Parcel(
    None, Some(model), Map.empty, None, None, None, Some(strategy), None, None, None, None
  )

  // def apply(command: Command, req: ServiceRequest): Parcel = Parcel(
  //   Some(command), command.getModel, None, None, Some(req)
  // )
  // def apply(command: Command, context: ExecutionContext): Parcel = Parcel(
  //   Some(command), command.getModel, None, Some(context), context.request
  // )
}
