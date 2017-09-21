package arcadia.view

import scala.xml._
import org.goldenport.exception.RAISE
import arcadia._
import arcadia.model._

/*
 * @since   Aug.  2, 2017
 * @version Sep. 20, 2017
 * @author  ASAMI, Tomoharu
 */
case class ViewModel(model: Model, strategy: RenderStrategy) {
  lazy val locale = strategy.locale
  private lazy val _view_engine = strategy.context.map(_.engine).getOrElse {
    RAISE.noReachDefect
  }

  /*
   * Render
   */
  def render: NodeSeq = model match {
    case m: IPageModel => render_html(m)
    case m: ISectionModel => render_section(m)
    case m: IComponentModel => render_component(m)
  }

  protected def render_html(p: IPageModel): NodeSeq = model.render(strategy.html)

  protected def render_section(p: ISectionModel with Model): NodeSeq = {
    val str = strategy.section
    val pagename = p.title.map(_.toI18NString)
    val headline = None
    val title = p.title
    val caption = None
    new Renderer(str, pagename, headline, title, caption) {
      protected def render_Content = RAISE.noReachDefect
      override def render_section: NodeSeq = p.render(str)
    }.apply
  }

  protected def render_component(p: IComponentModel with Model): NodeSeq = {
    val str = strategy.content
    val pagename = p.caption.map(_.toI18NString)
    val headline = None
    val title = None
    val caption = p.caption
    new Renderer(str, pagename, headline, title, caption) {
      protected def render_Content = p.render(str)
    }.apply
  }

  /*
   * Partial
   */
  def headDef: NodeSeq = _render_partial(strategy.partials.headDef)
  def footDef: NodeSeq = _render_partial(strategy.partials.footDef)
  def header: NodeSeq = _render_partial(strategy.partials.header)
  def footer: NodeSeq = _render_partial(strategy.partials.footer)
  def sidebar: NodeSeq = _render_partial(strategy.partials.sidebar)
  def sidebarContent: NodeSeq = strategy.theme.sidebar.content(this)
  def navigation: NodeSeq = _render_partial(strategy.partials.navigation)
  def navigationContent: NodeSeq = strategy.theme.navigation.content(this)
  def content: NodeSeq = _render_partial(strategy.partials.content)
  def contentContent: NodeSeq = model match {
    case m: ISectionModel => render_view_section(m)
    case m: IComponentModel => render_view_component(m)
    case _ => <div>No content</div>
  }
  def partial(p: PartialKind): NodeSeq = _render_partial(strategy.partials.get(p))

  private def _render_partial(p: Option[PartialView]): NodeSeq = 
    p.map { view =>
      strategy.context.fold {
        RAISE.noReachDefect
      } { c =>
        view.render(c.engine, c.parcel)
      }
    }.getOrElse(Group(Nil))

  protected def render_view_section(p: ISectionModel with Model): NodeSeq = {
    val parcel = Parcel(p, strategy)
    _view_engine.renderOption(parcel) getOrElse render_section(p)
  }

  protected def render_view_component(p: IComponentModel with Model): NodeSeq = {
    val parcel = Parcel(p, strategy)
    _view_engine.renderOption(parcel) getOrElse render_component(p)
  }

  /*
   * Attributes
   */
  def applicationTitle: NodeSeq = strategy.application.applicationTitle(locale)
  def applicationLogo: NodeSeq = strategy.application.applicationLogo(locale)
  def isActiveFeature(p: String): Boolean = model.isActiveFeature(p)

  /*
   * View
   */
  def entity(name: String): ViewEntityList = {
    model match {
      case m: IndexModel =>
        val a = m.getEntityList(name) getOrElse EntityListModel.empty(name)
        ViewEntityList(a, strategy)
      case m => RAISE.notImplementedYetDefect
    }
  }

  def assets: String =
    strategy.context.flatMap(_.parcel.context.map(_.assets)) getOrElse {
      "assets"
    }
}
