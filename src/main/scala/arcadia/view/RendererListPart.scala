package arcadia.view

import scala.xml.{NodeSeq, Group, Elem, Node, Text}
import arcadia.model._
import Renderer._

/*
 * @since   Apr. 15, 2018
 *  version May.  2, 2018
 * @version Sep.  1, 2018
 * @author  ASAMI, Tomoharu
 */
trait RendererListPart { self: Renderer =>
  def list_table(p: TableWithRecords): Elem = {
    val cards = p.records.map(to_card(p.table, _)).toList
    list_card(cards)
  }

  def list_card(ps: Seq[Card]): Elem = {
    strategy.theme match {
      case m: Bootstrap4RenderThemeBase => list_card_bootstrap(ps)
      case m => list_card_plain(ps)
    }
  }

  protected def list_card_bootstrap(ps: Seq[Card]): Elem = {
    <ul class="list-unstyled">{
      ps.map(list_card_bootstrap)
    }</ul>
  }

  protected def list_card_bootstrap(p: Card): Elem =
    <li class="media">{card_in_list_body(p)}</li>

  protected def list_card_plain(ps: Seq[Card]): Elem = {
    <ul>{
      ps.map(list_card_plain)
    }</ul>
  }

  protected def list_card_plain(p: Card): Elem =
    <li>{card_in_list(p)}</li>
}
