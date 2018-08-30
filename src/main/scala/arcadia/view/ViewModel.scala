package arcadia.view

import scala.xml._
import org.goldenport.exception.RAISE
import org.goldenport.values.PathName
import arcadia._
import arcadia.context.{Query => CQuery, ExecutionContext}
import arcadia.domain._
import arcadia.model._

/*
 * @since   Aug.  2, 2017
 *  version Sep. 30, 2017
 *  version Oct. 21, 2017
 *  version Jan. 21, 2018
 * @version Aug.  5, 2018
 * @author  ASAMI, Tomoharu
 */
case class ViewModel(model: Model, strategy: RenderStrategy) {
  lazy val locale = strategy.locale
  private lazy val _view_engine = strategy.viewContext.map(_.engine).getOrElse {
    RAISE.noReachDefect
  }

  /*
   * Render
   */
  def render: NodeSeq = model match {
    case m: IPageModel => render_page(m)
    case m: ISectionModel => render_section(m)
    case m: IComponentModel => render_component(m)
    case m: IAtomicModel => render_atomic(m)
    case EmptyModel => model.render(strategy)
  }

  protected def render_page(p: IPageModel): NodeSeq = model.render(strategy)

  protected def render_html(p: IPageModel): NodeSeq = model.render(strategy.withScopeHtml)

  protected def render_section(p: ISectionModel with Model): NodeSeq = {
    val str = strategy.withScopeSection
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
    val str = strategy.withScopeContent
    val pagename = p.caption.map(_.toI18NString)
    val headline = None
    val title = None
    val caption = p.caption
    new Renderer(str, pagename, headline, title, caption) {
      protected def render_Content = p.render(str)
    }.apply
  }

  protected def render_atomic(p: IAtomicModel with Model): NodeSeq = {
    val str = strategy.withScopeContent
    val pagename = None
    val headline = None
    val title = None
    val caption = None
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
  def contentHeader: NodeSeq = _render_partial(strategy.partials.contentHeader)
  def content: NodeSeq = _render_partial(strategy.partials.content)

  def contentContent: NodeSeq =
    strategy.viewContext.
      flatMap(_content_content_from_view).
      getOrElse(_content_content_from_model)

  private def _content_content_from_view(p: ViewContext): Option[NodeSeq] =
    p.parcel.view.map(_.render(strategy.withScopeContent))

  private def _content_content_from_model: NodeSeq = model match {
    case m: ISectionModel => render_view_section(m)
    case m: IComponentModel => render_view_component(m)
    case m: IAtomicModel => render_view_atomic(m)
    case EmptyModel => <div>No content</div>
  }
  def partial(p: PartialKind): NodeSeq = _render_partial(strategy.partials.get(p))

  private def _render_partial(p: Option[PartialView]): NodeSeq = 
    p.map { view =>
      strategy.viewContext.fold {
        RAISE.noReachDefect
      } { c =>
        view.render(c.engine, c.parcel)
      }
    }.getOrElse(Group(Nil))

  protected def render_view_section(p: ISectionModel with Model): NodeSeq = {
    val parcel = Parcel(p, strategy.withScopeContent)
    _view_engine.renderSectionOption(parcel) getOrElse render_section(p)
  }

  protected def render_view_component(p: IComponentModel with Model): NodeSeq = {
    val parcel = Parcel(p, strategy.withScopeContent)
    _view_engine.renderComponentOption(parcel) getOrElse render_component(p)
  }

  protected def render_view_atomic(p: IAtomicModel with Model): NodeSeq = {
    val parcel = Parcel(p, strategy.withScopeContent)
    _view_engine.renderAtomicOption(parcel) getOrElse render_atomic(p)
  }

  /*
   * Theme
   */
  def bodyClassName: String = strategy.theme.body.cssClass(strategy)

  /*
   * Attributes
   */
  def isLogined: Boolean = strategy.isLogined
  def applicationTitle: NodeSeq = strategy.application.applicationTitle(locale)
  def applicationLogo: NodeSeq = strategy.application.applicationLogo(locale)
  def isActiveFeature(p: String): Boolean = model.isActiveFeature(p)

  lazy val getExecutionContext: Option[ExecutionContext] = strategy.viewContext.flatMap(_.parcel.context)
  def resolvePathName(p: String): PathName = resolvePathName(PathName(p))
  def resolvePathName(pn: PathName): PathName = getExecutionContext.fold(pn)(_.resolvePathName(pn))
  def pageTitle: NodeSeq = strategy.getPage.map(_.title(locale)).getOrElse(Text("No title"))
  def pageContentHeaderStyle: String = strategy.getPage.flatMap(_.contentHeaderStyle).getOrElse("background-image: url('assets/img/bg37.jpg') ;") // TODO

  /*
   * View
   */
  def entityList(name: String): ViewEntityList = {
    model match {
      case m: IndexModel =>
        val a = m.getEntityList(name) getOrElse EntityListModel.empty(name)
        ViewEntityList(a, strategy)
      case m => RAISE.notImplementedYetDefect
    }
  }

  def assets: String =
    strategy.viewContext.flatMap(_.parcel.context.map(_.assets)) getOrElse {
      "assets"
    }
}
