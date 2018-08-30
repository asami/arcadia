package arcadia.view

import scala.xml.{NodeSeq, Group, Elem, Node, Text}
import org.goldenport.exception.RAISE
import org.goldenport.record.v2._
import org.goldenport.i18n.{I18NString, I18NElement}
import org.goldenport.xml.XmlUtils
import org.goldenport.util.{DateTimeUtils, DateUtils, StringUtils, SeqUtils}
import arcadia.model._
import Renderer._

/*
 * @since   May.  3, 2018
 * @version Jul.  2, 2018
 * @author  ASAMI, Tomoharu
 */
trait RendererTabsPart { self: Renderer =>
  protected def tabs(p: Tabs): Node = {
    tabs_bootstrap(p)
  }

  protected def tabs_bootstrap(p: Tabs): Node = {
    val ids = p.panes.map(x => x -> generate_id).toMap
    val ul = <ul class="nav nav-tabs">{
      p.panes match {
        case Nil => <div/>
        case x :: xs =>
          <li class="nav-item"><a class="nav-link active" data-toggle="tab" href={s"#${ids(x)}"}>{x.label(locale)}</a></li> +:
          xs.map(x => <li class="nav-item"><a class="nav-link" data-toggle="tab" href={s"#${ids(x)}"}>{x.label(locale)}</a></li>)
      }
    }</ul>
    val content = <div class="tab-content">{
      p.panes match {
        case Nil => RAISE.noReachDefect
        case x :: xs =>
          <div id={s"${ids(x)}"} class="tab-pane fade show active">{x.content.xml}</div> +:
          xs.map(x => <div id={s"${ids(x)}"} class="tab-pane fade">{x.content.xml}</div>)
      }
    }</div>
    Group(List(ul, content))
  }
}
