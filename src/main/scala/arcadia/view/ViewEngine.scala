package arcadia.view

import scala.xml.NodeSeq
import org.fusesource.scalate._
import org.fusesource.scalate.support.URLTemplateSource
import org.goldenport.record.v2._
import org.goldenport.value._
import org.goldenport.util.MapUtils
import arcadia._
import arcadia.context._

/*
 * @since   Jul.  8, 2017
 *  version Aug. 30, 2017
 * @version Sep. 21, 2017
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

  lazy val layouts: Map[LayoutKind, LayoutView] = MapUtils.complements(rule.layouts, extend.map(_.layouts))

  lazy val partials: Partials = rule.partials.complements(extend.map(_.partials))

  def findView(parcel: Parcel): Option[View] =
    slots.find(_.isAccept(parcel)).map(_.view)

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
      "import arcadia.model._"
    )
    a
  }

  // def apply(parcel: Parcel): Content = rule.findView(parcel).map(_.apply(this, parcel)) getOrElse {
  //   WebViewNotFoundFault(parcel.toMessage).RAISE
  // }

  def apply(p: Parcel): Content = applyOption(p) getOrElse {
    throw new IllegalStateException(p.toMessage) // WebViewNotFoundFault(p.toMessage).RAISE
  }

  def applyOption(p: Parcel): Option[Content] = {
    val render = {
      val t = theme getOrElse PlainTheme
      (p.render getOrElse PlainHtml).withThemePartials(t, partials)
    }
    val parcel = p.withRenderStrategy(render)
    findView(p).fold {
      extend.toStream.flatMap(_.applyOption(p)).headOption orElse {
        p.getEffectiveModel map { m =>
          getLayout(parcel).map(_.apply(this, parcel)) getOrElse {
            StringContent(MimeType.text_html, None, m.render(render).toString)
          }
        }
      }
    } { content =>
      val page = getLayout(parcel).getOrElse(content)
      Some(page.apply(this, parcel))
    }
  }

  def renderOption(p: Parcel): Option[NodeSeq] = findView(p).
    fold(
      extend.toStream.flatMap(_.renderOption(p)).headOption
    )(page =>
      Some(page.render(this, p)))

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
}
object ViewEngine {
  final val PROP_VIEW_MODEL = "model"
  final val PROP_VIEW_OBJECT = "o"
  final val PROP_VIEW_LIST = "list"
  final val PROP_VIEW_RECORD = "record"
  final val PROP_VIEW_RECORDS = "records"

  case class Rule(
    theme: Option[RenderTheme],
    slots: Vector[ViewEngine.Slot], // pages, components
    layouts: Map[LayoutKind, LayoutView],
    partials: Partials,
    components: Components
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
      theme: RenderTheme,
      slots: Seq[Slot],
      layouts: Map[LayoutKind, LayoutView],
      partials: Partials,
      components: Components
    ): Rule = Rule(Some(theme), slots.toVector, layouts, partials, components)

    def create(head: (Guard, View), tail: (Guard, View)*): Rule = Rule(
      None,
      (head +: tail.toVector).map(Slot(_)),
      Map.empty,
      Partials.empty,
      Components.empty
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
