package arcadia.view

import scala.xml._
import org.fusesource.scalate._
import org.fusesource.scalate.support.URLTemplateSource
import org.goldenport.exception.RAISE
import org.goldenport.record.v2._
import org.goldenport.value._
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
 * @version Nov.  6, 2017
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

  lazy val layouts: Map[LayoutKind, LayoutView] = MapUtils.complements(rule.layouts, extend.map(_.layouts))

  lazy val partials: Partials = rule.partials.complements(extend.map(_.partials))

  lazy val pages: Pages = rule.pages.complements(extend.map(_.pages))

  lazy val tags: Tags = rule.tags.complements(extend.map(_.tags)).complements(Tags.embeded)

  def findView(parcel: Parcel): Option[View] =
    slots.find(_.isAccept(parcel)).map(_.view)

  def findComponent(parcel: Parcel): Option[View] =
    components.find(_.isAccept(parcel)).map(_.view)

  def getLayout(parcel: Parcel): Option[LayoutView] = {
    val uselayout = is_spa(parcel) || parcel.command.map(_.getUseLayout.getOrElse(true)).getOrElse(true)
    if (uselayout)
      layouts.get(DefaultLayout)
    else
      None
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

  def applyOption(p: Parcel): Option[Content] = {
    val render = {
      val t = theme getOrElse PlainTheme
      (p.render getOrElse PlainHtml).withThemePartials(t, partials)
    }
    val parcel = p.withRenderStrategy(render)
    findView(parcel).fold {
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
      val a: Option[Content] = content match {
        case m: MaterialView => m.getControlContent(parcel)
        case _ => None
      }
      a orElse {
        val page = getLayout(parcel).getOrElse(content)
        Some(page.apply(this, parcel.withView(content)))
      }
    }
  }

  private def _apply_option(p: Parcel): Option[Content] = findView(p).
    fold(
      extend.toStream.flatMap(_._apply_option(p)).headOption
    )(page =>
      Some(page.apply(this, p)))

  def applySectionOption(p: Parcel): Option[Content] = {
    val parcel = p.sectionScope
    _apply_option(parcel)
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
      page.apply(this, p.withModel(m).withView(x))
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
    _template_engine.layoutAsNodes(template.uri, bindings)
  }

  def eval(parcel: Parcel, p: Content): Content = _tag_engine.call(parcel).apply(p)

}
object ViewEngine {
  final val PROP_VIEW_MODEL = "model"
  final val PROP_VIEW_SERVICE = "service"
  final val PROP_VIEW_OBJECT = "o"
  final val PROP_VIEW_LIST = "list"
  final val PROP_VIEW_RECORD = "record"
  final val PROP_VIEW_RECORDS = "records"

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
    // def findView(parcel: Parcel): Option[View] =
    //   slots.find(_.isAccept(parcel)).map(_.view)

    // def getLayout(parcel: Parcel): Option[LayoutView] = {
    //   val uselayout = parcel.command.flatMap(_.getUseLayout).getOrElse(true)
    //   if (uselayout)
    //     layouts.get(DefaultLayout)
    //   else
    //     None
    // }
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
    val elements = Vector(DefaultLayout)
  }
  case object DefaultLayout extends LayoutKind {
    val name = "default"
  }
}
