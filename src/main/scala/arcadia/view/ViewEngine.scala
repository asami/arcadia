package arcadia.view

import scala.xml._
import java.io.File
import org.fusesource.scalate._
import org.fusesource.scalate.util.FileResource
import org.goldenport.exception.RAISE
import org.goldenport.record.v2._
import org.goldenport.value._
import org.goldenport.values.PathName
import org.goldenport.io.IoUtils
import org.goldenport.trace.Result
import org.goldenport.util.MapUtils
import arcadia._
import arcadia.context._
import arcadia.view.tag._
import arcadia.view.expression.ExpressionEngine
import arcadia.model.{Model, ErrorModel}

/*
 * @since   Jul.  8, 2017
 *  version Aug. 30, 2017
 *  version Sep. 30, 2017
 *  version Oct. 31, 2017
 *  version Nov. 15, 2017
 *  version Dec. 18, 2017
 *  version Jan.  7, 2018
 *  version Jul. 28, 2018
 *  version Mar. 21, 2020
 *  version Mar. 28, 2022
 *  version Apr. 30, 2022
 *  version May. 22, 2022
 *  version Sep. 10, 2022
 *  version Nov. 27, 2022
 *  version Nov. 28, 2023
 *  version Dec. 28, 2023
 *  version Mar. 29, 2025
 * @version Apr.  3, 2025
 * @author  ASAMI, Tomoharu
 */
class ViewEngine(
  val platform: PlatformContext,
  val rule: ViewEngine.Rule,
  val extend: List[ViewEngine],
  val templateEngines: TemplateEngineHangar
) {
  import ViewEngine._

  lazy val theme: Option[RenderTheme] =
    rule.theme orElse extend.toStream.flatMap(_.theme).headOption

  lazy val slots: Vector[Slot] = rule.slots ++ extend.toVector.flatMap(_.slots)

  private var _new_slots: Vector[Slot] = Vector.empty

  lazy val components: Vector[Slot] = {
    val a: Vector[Slot] = rule.components.components
    val b: Vector[Slot] = extend.toVector.flatMap(_.components)
    a ++ b
  }

//  lazy val layouts: Map[LayoutKind, LayoutView] = MapUtils.complements(rule.layouts, extend.map(_.layouts))

  lazy val partials: Partials = rule.partials.complements(extend.map(_.partials))

  lazy val pages: Pages = rule.pages.complements(extend.map(_.pages))

  lazy val tags: Tags = rule.tags.complements(extend.map(_.tags)).complements(Tags.embeded)

  def findView(parcel: Parcel): Option[View] =
    slots.find(_.isAccept(parcel)).map(_.view) orElse _find_new_view(parcel)

  private def _find_new_view(parcel: Parcel): Option[View] =
    _new_slots.find(_.isAccept(parcel)).map(_.view) orElse {
      val r = parcel.command.flatMap {
        case MaterialCommand(pathname) => _get_view(pathname)
        case _ => None
      }
      r foreach { x =>
        _new_slots = _new_slots :+ Slot(x.gv)
      }
      r
    }

  private def _get_view(pathname: PathName): Option[View] =
    if (true) { // disable in production
      for {
        dir <- rule.baseDir
        candidate <- _find_candidate(pathname, dir.listFiles())
        view <- _get_view(candidate)
      } yield view
    } else {
      None
    }

  private def _find_candidate(pn: PathName, ps: Seq[File]): Option[File] = {
    val s = pn.v + "."
    ps.find(_.getName.startsWith(s))
  }

  private def _get_view(p: File): Option[View] = {
    val pn = p.getName
    if (WebModule.isHtml(pn))
      Some(HtmlView(p))
    else if (WebModule.isTemplate(pn))
      Some(_template_view(p))
    else
      None
  }

  private def _template_view(p: File): PageView = PageView.create(p)

  def findComponent(parcel: Parcel): Option[View] = {
    val a = components.find(_.isAccept(parcel))
    val b = a orElse parcel.render.flatMap(_.components.components.find(_.isAccept(parcel)))
    b.map(_.view)
  }

  def getLayout(parcel: Parcel): Option[LayoutView] = {
    val layout =
      if (is_spa(parcel))
        Some(DefaultLayout)
      else
        parcel.command.flatMap(_.getLayout).orElse(
          rule.getLayoutKind(parcel).orElse(
            extend.toStream.flatMap(_.rule.getLayoutKind(parcel)).headOption
          )
        )
    layout match {
      case Some(NoneLayout) => None
      case Some(m) => getLayoutOrDefault(m)
      case None =>
        parcel.command match {
          case Some(s) => s match {
            case m: MaterialCommand =>
              val pathname = m.pathname
              if (pathname.leafBody.toLowerCase == "index")
                if (pathname.length == 1)
                  getLayoutOrDefault(HomepageLayout)
                else
                  getLayoutOrDefault(IndexLayout)
              else
                getDefaultLayout()
            case _ => getDefaultLayout()
          }
          case None => getDefaultLayout()
        }
    }
    // val uselayout = is_spa(parcel) || parcel.command.map(_.getUseLayout.getOrElse(true)).getOrElse(true)
    // if (uselayout)
    //   layouts.get(DefaultLayout)
    // else
    //   None
  }

  def getLayoutOrDefault(kind: LayoutKind): Option[LayoutView] = getLayout(kind) orElse getDefaultLayout()

  def getDefaultLayout(): Option[LayoutView] = getLayout(DefaultLayout)

  def getLayout(kind: LayoutKind): Option[LayoutView] = rule.getLayout(kind).orElse(
    extend.toStream.flatMap(_.getLayout(kind)).headOption
  )

  protected def is_spa(parcel: Parcel): Boolean = parcel.command.map {
    case m: MaterialCommand => rule.isSinglePageApplication(m.pathname.v)
    case m => false
  }.getOrElse(false)

  protected def is_spa_redirect(parcel: Parcel): Boolean = 
    is_spa(parcel) && parcel.command.map {
      case m: MaterialCommand => !rule.isSinglePageApplicationRoot(m.pathname.v)
      case m => false
    }.getOrElse(false)

  private val _template_engine = templateEngines // new ScalateTemplateEngine(platform)
  private val _tag_engine = new TagEngine(tags)
  private val _expression_engine = new ExpressionEngine()

  def apply(p: Parcel): Content = applyOption(p) getOrElse {
    error(p, 404)
  }

  def applyOption(p: Parcel): Option[Content] = p.executeWithTrace("ViewEngine#applyOption", p.show) {
    val render = {
      val t = p.getTheme orElse theme getOrElse PlainTheme
      // val style = "MM" // TODO
      // val f = FormatterContext.createStyle(style)
      val f = p.context.fold(FormatterContext.default)(x => FormatterContext.create(x))
      (p.render getOrElse PlainHtml).
        withThemeComponentsPartials(t, Components(components), partials).
        withFormatter(f)
    }
    val parcel = p.withRenderStrategy(render)
    _apply_option_recursive(parcel)
  }

  def applyOptionParent(p: Parcel): Option[Content] = p.executeWithTrace("ViewEngine#applyOptionParent", p.show) {
    _apply_option_recursive(p)
  }

  private def _apply_option_recursive(parcel: Parcel) = {
    def _render_ = parcel.render getOrElse RAISE.noReachDefect
    val r1 = findView(parcel).fold {
      extend.toStream.flatMap(_.applyOptionParent(parcel)).headOption orElse {
        // val model = p.getEffectiveModel orElse Some(ErrorModel.notFound(parcel, "View and Model is not found."))
        // model map { m =>
        //   getLayout(parcel).map(_.apply(this, parcel)) getOrElse {
        //     m.apply(render)
        //   }
        // }
        def f(m: Model): Content = getLayout(parcel).
          map(_.apply(this, parcel)).
          getOrElse(m.apply(_render_))
        parcel.getEffectiveModel.map(f).
          orElse(
            if (is_spa_redirect(parcel))
              rule.
              getRedirectCommandForSinglePageApplication.
              flatMap(x => applyOption(parcel.withCommand(x)))
            else
              // Some(f(ErrorModel.notFound(parcel, "View and Model is not found.")))
              None
          )
      }
    } { content =>
      def go = {
        val page = getLayout(parcel).getOrElse(content)
        Some(page.apply(this, parcel.withView(content)))
      }
      content match {
        case m: MaterialView =>
          m.getControlContent(parcel) orElse {
            m.apply(this, parcel) match {
              case m: BinaryContent => Some(m)
              case m: StringContent => Some(m) // TODO layout
              case _ => go
            }
          }
        case _ => go
      }
      // val a: Option[Content] = content match {
      //   case m: MaterialView => m.getControlContent(parcel)
      //   case _ => None
      // }
      // a orElse {
      //   val page = getLayout(parcel).getOrElse(content)
      //   Some(page.apply(this, parcel.withView(content)))
      // }
    }
    val r: Option[Content] = {
      val a: Option[Content] = r1.flatMap {
        case m: XmlContent =>
          parcel.render.flatMap(_.renderContext.epilogue.flatMap(
            _.getScriptElement).map(m.addScriptElement)
          )
        case _ => None
      }
      a.orElse(r1)
    }
    Result(r, r.map(_.show))
  }

  private def _apply_option(p: Parcel): Option[Content] = findView(p).
    fold(
      extend.toStream.flatMap(_._apply_option(p)).headOption
    )(page =>
      Some(page.apply(this, p)))

  def applySectionOption(p: Parcel): Option[Content] = p.executeWithTrace("ViewEngine#applySectionOption", p.show) {
    val parcel = p.sectionScope
    val r = _apply_option(parcel)
    Result(r, r.map(_.show))
  }

  def applyComponentOption(p: Parcel): Option[Content] = {
    val parcel = p.componentScope
    findComponent(parcel).
      fold(
        extend.toStream.flatMap(_.applyComponentOption(parcel)).headOption
      )(page =>
        Some(page.apply(this, parcel)))
  }

  def applyAtomicOption(p: Parcel): Option[Content] = {
    val parcel = p.componentScope
    _apply_option(parcel)
  }

  def error(p: Parcel, code: Int): Content = error(p, ErrorModel.create(p, code))
  def error(p: Parcel, e: Throwable): Content = error(p, ErrorModel.create(p, 503, e))
  def error(p: Parcel, m: ErrorModel): Content = { // TODO layout
    def name(code: Int) = s"error/${code}"
    def default = "error/default"
    (pages.get(name(m.code)) orElse pages.get(default)).map { x =>
      // val page = getLayout(parcel).getOrElse(content)
      val page = x
      page.apply(this, p.withModel(m).withView(x)).withCode(m.code)
    }.getOrElse {
      p.render match {
        case Some(s) => m.apply(s)
        case None => m.RAISE
      }
    }
  }

  def renderSectionOption(p: Parcel): Option[NodeSeq] = applySectionOption(p).map(_.asXml)
  def renderComponentOption(p: Parcel): Option[NodeSeq] = applyComponentOption(p).map(_.asXml)
  def renderAtomicOption(p: Parcel): Option[NodeSeq] = applyAtomicOption(p).map(_.asXml)

  def shutdown(): Unit = _template_engine.shutdown()

  // private def layout(template: TemplateSource, bindings: Map[String, Object]): String =
  //   _template_engine.layout(template, bindings)

  def render(template: TemplateSource, parcel: Parcel, bindings: Bindings): NodeSeq = {
    // val keys = bindings.keySet
    // val meta = Vector(
    //   Binding(PROP_VIEW_MODEL, "_root_.arcadia.view.ViewModel", true, None, "val", false),
    //   Binding(PROP_VIEW_SERVICE, "_root_.arcadia.view.ViewService", true, None, "val", false),
    //   Binding(PROP_VIEW_OBJECT, "_root_.arcadia.view.ViewObject", true, None, "val", false),
    //   Binding(PROP_VIEW_LIST, "_root_.arcadia.view.ViewList", true, None, "val", false),
    //   Binding(PROP_VIEW_CARD, "_root_.arcadia.view.ViewCard", true, None, "val", false),
    //   Binding(PROP_VIEW_RECORD, "_root_.arcadia.view.ViewRecord", true, None, "val", false)
    //  // Binding(PROP_VIEW_RECORD, "_root_.arcadia.view.ViewRecords", true, None, "val", false)
    // ).filter(x => keys.contains(x.name))
    // _template_engine.layoutAsNodes(template.uri, bindings, meta)
    implicit val pec = parcel.getPlatformExecutionContext getOrElse RAISE.noReachDefect
    val r = _template_engine.layoutAsNodes(template, bindings)
    // if (true)
    //   _template_engine.invalidateCachedTemplates()
    // r
    r
  }

  def eval(parcel: Parcel, p: Content, bindings: Bindings): Content = {
    val a = _tag_engine.call(parcel, bindings).apply(p)
    _expression_engine.apply(parcel, bindings, a)
  }
}
object ViewEngine {
  // Scalate uses variable context.
  // final val PROP_VIEW_CONTEXT = "context"
  final val PROP_VIEW_IT = "it"
  final val PROP_VIEW_MODEL = "model"
  final val PROP_VIEW_SERVICE = "service"
  final val PROP_VIEW_OBJECT = "o"
  final val PROP_VIEW_LIST = "list"
  final val PROP_VIEW_CARD = "card"
  final val PROP_VIEW_RECORD = "record"
  final val PROP_VIEW_RECORDS = "records"
  final val PROP_VIEW_WIDGET = "widget"
  final val PROP_VIEW_FORM = "form"
  final val PROP_VIEW_PROPERTIES = "properties"

  case class Rule(
    theme: Option[RenderTheme],
    slots: Vector[ViewEngine.Slot], // pages, components
    layouts: Map[LayoutKind, LayoutView],
    partials: Partials,
    pages: Pages,
    components: Components,
    tags: Tags,
    singlePageApplication: Option[WebApplicationRule.SinglePageApplication],
    baseDir: Option[File]
  ) {
    def isSinglePageApplication(pathname: String): Boolean =
      singlePageApplication.map(_.isActive(pathname)).getOrElse(false)

    def getRedirectCommandForSinglePageApplication: Option[MaterialCommand] =
      singlePageApplication.map(_.redirectCommand)

    def isSinglePageApplicationRoot(p: String): Boolean =
      singlePageApplication.map(_.isRootPagePathname(p)).getOrElse(false)

    // TODO config
    private val _operation_kind = Map(
      "login" -> PlainLayout,
      "logout" -> PlainLayout
    )

    def getLayoutKind(parcel: Parcel): Option[LayoutKind] = {
      def bymodel = parcel.model.flatMap {
        case m: ErrorModel => Some(ErrorLayout)
        case _ => None
      }
      def byoperation = parcel.getOperationName.flatMap(_operation_kind.get)
      bymodel orElse byoperation
    }

    def getLayout(kind: LayoutKind): Option[LayoutView] = layouts.get(kind)
  }
  object Rule {
    val error = {
      val theme = None
      val slots = Vector.empty
      val layouts = Map.empty[LayoutKind, LayoutView]
      val partials = Partials.empty
      val pages = Pages.empty
      val components = Components.empty
      val tags = Tags.empty
      val singlePageApplication = None
      val baseDir = None
      Rule(theme, slots, layouts, partials, pages, components, tags, singlePageApplication, baseDir)
    }

    def create(
      theme: Option[RenderTheme],
      slots: Seq[Slot],
      layouts: Map[LayoutKind, LayoutView],
      partials: Partials,
      pages: Pages,
      components: Components,
      tags: Tags,
      spa: Option[WebApplicationRule.SinglePageApplication],
      basedir: Option[File]
    ): Rule = Rule(theme, slots.toVector, layouts, partials, pages, components, tags, spa, basedir)

    def create(head: (Guard, View), tail: (Guard, View)*): Rule = Rule(
      None,
      (head +: tail.toVector).map(Slot(_)),
      Map.empty,
      Partials.empty,
      Pages.empty,
      Components.empty,
      Tags.empty,
      None,
      None
    )
  }

  case class Slot(guard: Guard, view: View) {
    def isAccept(parcel: Parcel): Boolean = guard.isAccept(parcel)
  }
  object Slot {
    def apply(p: (Guard, View)): Slot = Slot(p._1, p._2)
  }

  sealed trait LayoutKind extends NamedValueInstance
  object LayoutKind extends EnumerationClass[LayoutKind] {
    val elements = Vector(
      DefaultLayout,
      NoneLayout,
      ErrorLayout,
      PlainLayout,
      HomepageLayout,
      IndexLayout,
      ArticleLayout
    )
  }
  case object DefaultLayout extends LayoutKind {
    val name = "default"
  }
  case object NoneLayout extends LayoutKind {
    val name = "none"
  }
  case object ErrorLayout extends LayoutKind {
    val name = "error"
  }
  case object PlainLayout extends LayoutKind {
    val name = "plain"
  }
  case object HomepageLayout extends LayoutKind {
    val name = "homepage"
  }
  case object IndexLayout extends LayoutKind {
    val name = "index"
  }
  case object ArticleLayout extends LayoutKind {
    val name = "article"
  }
  case class PageLayout(name: String) extends LayoutKind {
  }

  case class Bindings(
    bindings: Map[String, AnyRef]
  ) {
    import scala.collection.JavaConverters._

    def viewModel: ViewModel = _get_view_model_keys("model", "it", "view") getOrElse RAISE.noReachDefect

    private def _get_view_model_keys(key: String, keys: String*): Option[ViewModel] =
      (key +: keys).toStream.flatMap(_get_view_model).headOption

    private def _get_view_model(key: String): Option[ViewModel] =
      bindings.get(key).flatMap {
        case m: ViewModel => Some(m)
        case m => None
      }

    def javaMap: java.util.Map[String, Object] = bindings.asJava
  }

  def evalExpression(
    p: TemplateSource,
    bindings: Bindings
  )(implicit pec: PlatformExecutionContext): TemplateSource = {
    p match {
      case m: FileResource => evalExpressionFile(m, bindings)
    }
  }

  def evalExpressionFile(
    p: FileResource,
    bindings: Bindings
  )(implicit pec: PlatformExecutionContext): TemplateSource = {
    val uri = p.uri
    val x = IoUtils.toText(p.file, pec.charsetInputFile)
    val s = evalExpression(x, bindings)
    TemplateSource.fromText(uri, s)
  }

  def evalExpression(s: String, bindings: Bindings): String = {
    import org.apache.commons.jexl3._
    import scala.util.matching.Regex
    val context = new MapContext(bindings.javaMap)
    val jexl = new JexlBuilder().create()
    val pattern: Regex = """\$\{([^}]+)\}""".r
    pattern.replaceAllIn(s, m => {
      val expr = jexl.createExpression(m.group(1).trim) 
      val result = expr.evaluate(context)
      result.toString
    })
  }
}
