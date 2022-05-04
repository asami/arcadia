package arcadia.view

import scala.xml._
import org.fusesource.scalate._
import org.fusesource.scalate.support.URLTemplateSource
import org.goldenport.exception.RAISE
import org.goldenport.record.v2._
import org.goldenport.value._
import org.goldenport.trace.Result
import org.goldenport.util.MapUtils
import arcadia._
import arcadia.context._
import arcadia.view.tag._
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
 * @version May.  2, 2022
 * @author  ASAMI, Tomoharu
 */
class ViewEngine(
  val platform: PlatformContext,
  val rule: ViewEngine.Rule,
  val extend: List[ViewEngine]
) {
  import ViewEngine._

  lazy val theme: Option[RenderTheme] =
    rule.theme orElse extend.toStream.flatMap(_.theme).headOption

  lazy val slots: Vector[Slot] = rule.slots ++ extend.toVector.flatMap(_.slots)

  lazy val components: Vector[Slot] = {
    val a: Vector[Slot] = rule.components.toSlots
    val b: Vector[Slot] = extend.toVector.flatMap(_.components)
    a ++ b
  }

//  lazy val layouts: Map[LayoutKind, LayoutView] = MapUtils.complements(rule.layouts, extend.map(_.layouts))

  lazy val partials: Partials = rule.partials.complements(extend.map(_.partials))

  lazy val pages: Pages = rule.pages.complements(extend.map(_.pages))

  lazy val tags: Tags = rule.tags.complements(extend.map(_.tags)).complements(Tags.embeded)

  def findView(parcel: Parcel): Option[View] =
    slots.find(_.isAccept(parcel)).map(_.view)

  def findComponent(parcel: Parcel): Option[View] =
    components.find(_.isAccept(parcel)).map(_.view)

  def getLayout(parcel: Parcel): Option[LayoutView] = {
    def getlayout(kind: LayoutKind) = rule.getLayout(kind).orElse(
      extend.toStream.flatMap(_.rule.getLayout(kind)).headOption
    )
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
      case Some(m) => getlayout(m) orElse getlayout(DefaultLayout)
      case None => getlayout(DefaultLayout)
    }
    // val uselayout = is_spa(parcel) || parcel.command.map(_.getUseLayout.getOrElse(true)).getOrElse(true)
    // if (uselayout)
    //   layouts.get(DefaultLayout)
    // else
    //   None
  }

  protected def is_spa(parcel: Parcel): Boolean = parcel.command.map {
    case m: MaterialCommand => rule.isSinglePageApplication(m.pathname.v)
    case m => false
  }.getOrElse(false)

  protected def is_spa_redirect(parcel: Parcel): Boolean = 
    is_spa(parcel) && parcel.command.map {
      case m: MaterialCommand => !rule.isSinglePageApplicationRoot(m.pathname.v)
      case m => false
    }.getOrElse(false)

  private val _template_engine = {
    val a = new TemplateEngine()
    a.workingDirectory = platform.createTempDirectory()
    // a.workingDirectory = { // TODO
    //   val tmp = new java.io.File("/tmp")
    //   val f = java.io.File.createTempFile("everforth", ".dir", tmp)
    //   f.delete
    //   f.mkdirs
    //   f
    // }
    a.mode = "develop"
    a.allowReload = true // TODO
//    a.escapeMarkup = false // style="background-image: url('assets/img/bg37.jpg') ;"
    a.importStatements = a.importStatements ::: List(
      "import arcadia.view._",
      "import arcadia.model._",
      "import arcadia.domain._"
    )
    a
  }

  private val _tag_engine = new TagEngine(tags)

  // def apply(parcel: Parcel): Content = rule.findView(parcel).map(_.apply(this, parcel)) getOrElse {
  //   WebViewNotFoundFault(parcel.toMessage).RAISE
  // }

  def apply(p: Parcel): Content = applyOption(p) getOrElse {
    error(p, 404)
  }

  def applyOption(p: Parcel): Option[Content] = p.executeWithTrace("ViewEngine#applyOption", p.show) {
    val render = {
      val t = theme getOrElse PlainTheme
      // val style = "MM" // TODO
      // val f = FormatterContext.createStyle(style)
      val f = p.context.fold(FormatterContext.default)(x => FormatterContext.create(x))
      (p.render getOrElse PlainHtml).
        withThemePartials(t, partials).
        withFormatter(f)
    }
    val parcel = p.withRenderStrategy(render)
    val r1 = findView(parcel).fold {
      extend.toStream.flatMap(_.applyOption(parcel)).headOption orElse {
        // val model = p.getEffectiveModel orElse Some(ErrorModel.notFound(parcel, "View and Model is not found."))
        // model map { m =>
        //   getLayout(parcel).map(_.apply(this, parcel)) getOrElse {
        //     m.apply(render)
        //   }
        // }
        def f(m: Model): Content = getLayout(parcel).
          map(_.apply(this, parcel)).
          getOrElse(m.apply(render))
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
      p.render.fold(RAISE.noReachDefect)(m.apply)
    }
  }

  def renderSectionOption(p: Parcel): Option[NodeSeq] = applySectionOption(p).map(_.asXml)
  def renderComponentOption(p: Parcel): Option[NodeSeq] = applyComponentOption(p).map(_.asXml)
  def renderAtomicOption(p: Parcel): Option[NodeSeq] = applyAtomicOption(p).map(_.asXml)

  def shutdown(): Unit = _template_engine.shutdown()

  def layout(template: TemplateSource, bindings: Map[String, Object]): String =
    _template_engine.layout(template, bindings)

  def render(template: TemplateSource, bindings: Map[String, Object]): NodeSeq = {
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
    _template_engine.layoutAsNodes(template.uri, bindings)
  }

  def eval(parcel: Parcel, p: Content): Content = _tag_engine.call(parcel).apply(p)

}
object ViewEngine {
  // Scalate uses variable context.
  // final val PROP_VIEW_CONTEXT = "context"
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
    singlePageApplication: Option[WebApplicationRule.SinglePageApplication]
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
    def create(
      theme: Option[RenderTheme],
      slots: Seq[Slot],
      layouts: Map[LayoutKind, LayoutView],
      partials: Partials,
      pages: Pages,
      components: Components,
      tags: Tags,
      spa: Option[WebApplicationRule.SinglePageApplication]
    ): Rule = Rule(theme, slots.toVector, layouts, partials, pages, components, tags, spa)

    def create(head: (Guard, View), tail: (Guard, View)*): Rule = Rule(
      None,
      (head +: tail.toVector).map(Slot(_)),
      Map.empty,
      Partials.empty,
      Pages.empty,
      Components.empty,
      Tags.empty,
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
      PlainLayout
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
  case class PageLayout(name: String) extends LayoutKind {
  }
}
