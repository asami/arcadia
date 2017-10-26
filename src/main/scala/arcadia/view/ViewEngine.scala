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
import arcadia.model.ErrorModel

/*
 * @since   Jul.  8, 2017
 *  version Aug. 30, 2017
 *  version Sep. 30, 2017
 * @version Oct. 27, 2017
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

  lazy val tags: Tags = rule.tags.complements(extend.map(_.tags)).complements(Tags.embeded)

  def findView(parcel: Parcel): Option[View] =
    slots.find(_.isAccept(parcel)).map(_.view)

  def findComponent(parcel: Parcel): Option[View] =
    components.find(_.isAccept(parcel)).map(_.view)

  def getLayout(parcel: Parcel): Option[LayoutView] = {
    val uselayout = parcel.command.flatMap(_.getUseLayout).getOrElse(true)
    if (uselayout)
      layouts.get(DefaultLayout)
    else
      None
  }

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
    throw new IllegalStateException(p.toMessage) // TODO WebViewNotFoundFault(p.toMessage).RAISE
  }

  def applyOption(p: Parcel): Option[Content] = {
    val render = {
      val t = theme getOrElse PlainTheme
      (p.render getOrElse PlainHtml).withThemePartials(t, partials)
    }
    val parcel = p.withRenderStrategy(render)
    findView(p).fold {
      extend.toStream.flatMap(_.applyOption(p)).headOption orElse {
        val model = p.getEffectiveModel orElse Some(ErrorModel.notFound(parcel, "View and Model is not found."))
        model map { m =>
          getLayout(parcel).map(_.apply(this, parcel)) getOrElse {
            m.apply(render)
          }
        }
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

  def renderSectionOption(p: Parcel): Option[NodeSeq] = applySectionOption(p).map(_.asXml)
  def renderComponentOption(p: Parcel): Option[NodeSeq] = applyComponentOption(p).map(_.asXml)
  def renderAtomicOption(p: Parcel): Option[NodeSeq] = applyAtomicOption(p).map(_.asXml)

  def shutdown(): Unit = _template_engine.shutdown()

  def layout(template: TemplateSource, bindings: Map[String, Object]): String =
    _template_engine.layout(template, bindings)

  def render(template: TemplateSource, bindings: Map[String, Object]): NodeSeq = {
    _template_engine.layoutAsNodes(template.uri, bindings)
  }

  // def render(r: Record): String = {
  //   val source = {
  //     if (true) {
  //       val filename = "/Users/asami/src/Project2017/EverforthFramework/src/main/resources/com/everforth/everforth/view/plain/detail.jade"
  //       TemplateSource.fromFile(filename)
  //     } else {
  //       val url = getClass.getResource("plain/detail.jade")
  //       new URLTemplateSource(url)
  //     }
  //   }
  //   val bindings = Map(PROP_VIEW_OBJECT -> ViewObject.create(r))
  //   _template_engine.layout(source, bindings)
  // }
  // def render(rs: RecordSet)(implicit context: ExecutionContext): String = render(rs.records)
  // def render(rs: Seq[Record])(implicit context: ExecutionContext): String = {
  //   val source = {
  //     if (true) {
  //       val filename = "/Users/asami/src/Project2017/EverforthFramework/src/main/resources/com/everforth/everforth/view/plain/list.jade"
  //       TemplateSource.fromFile(filename)
  //     } else {
  //       val url = getClass.getResource("plain/list.jade")
  //       new URLTemplateSource(url)
  //     }
  //   }
  //   val bindings = Map(PROP_VIEW_LIST -> rs.map(ViewObject.create).toList)
  //   _template_engine.layout(source, bindings)
  // }

  def eval(parcel: Parcel, p: Content): Content = _tag_engine.call(parcel).apply(p)

  // def eval(p: NodeSeq): NodeSeq = p match {
  //   case m: Text => m
  //   case m: Elem =>
  //     val xs = m.child.flatMap(_eval)
  //     _eval_element(m, xs) getOrElse Group(Nil)
  //   case m if m.length > 0 => Group(m.toList.flatMap(_eval))
  //   case m => m
  // }

  // private def _eval(p: Node): Option[Node] = p match {
  //   case m: Text => Some(m)
  //   case m: Elem =>
  //     val xs = m.child.flatMap(_eval)
  //     _eval_element(m, xs)
  //   case m if m.length > 0 => Some(Group(m.toList.flatMap(_eval)))
  //   case m => Some(m)
  // }

  // private def _eval_element(p: Elem, children: Seq[Node]): Option[Node] = {
  //   if (p.label == "model")
  //     Some(Text("MODEL Component"))
  //   else
  //     Some(p.copy(child = children))
  // }
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
    components: Components,
    tags: Tags
  ) {
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
      components: Components,
      tags: Tags
    ): Rule = Rule(theme, slots.toVector, layouts, partials, components, tags)

    def create(head: (Guard, View), tail: (Guard, View)*): Rule = Rule(
      None,
      (head +: tail.toVector).map(Slot(_)),
      Map.empty,
      Partials.empty,
      Components.empty,
      Tags.empty
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
