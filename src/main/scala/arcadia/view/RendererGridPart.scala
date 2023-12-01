package arcadia.view

import scala.xml.{NodeSeq, Group, Elem, Node, Text}
import org.goldenport.xml.XmlUtils
import arcadia.model._
import Renderer._

/*
 * @since   Apr. 15, 2018
 *  version Apr. 15, 2018
 * @version Nov. 28, 2023
 * @author  ASAMI, Tomoharu
 */
trait RendererGridPart { self: Renderer =>
  protected def grid(p: TableWithRecords): Elem = {
    val cards = p.records.map(to_card(p.table, _)).toList
    grid(cards)
    // val grid = strategy.gridContext
    // val ncolumns = 6
    // val width = 12 / ncolumns
    // <div class="container"> { // container-fluid
    //   for (row <- p.records.grouped(ncolumns)) yield {
    //     <div class="row"> {
    //       for (x <- row) yield {
    //         <div class={s"col-sm-${width}"}> {
    //           card(x)
    //         } </div>
    //       }
    //     } </div>
    //   }
    // } </div>
  }

  protected def grid(ps: List[Card]): Elem =
    if (ps.length == 0)
      empty_block
    else
      grid_with_content(ps)(card_in_grid(_))

  protected def grid_with_content[T](ps: List[T])(f: T => NodeSeq): Elem =
    strategy.theme match {
      case m: Bootstrap5RenderThemeBase => grid_bootstrap4(ps)(f)
      case m: Bootstrap4RenderThemeBase => grid_bootstrap4(ps)(f)
      case m: Bootstrap3RenderThemeBase => grid_bootstrap3(ps)(f)
      case m if (m.isGridDiv) => grid_div(ps)(f)
      case m if (m.isGridTable) => grid_table(ps)(f)
      case m => grid_table(ps)(f)
    }

  protected def grid_with_content_bootstrap[T](ps: List[T])(f: T => NodeSeq): Elem =
    strategy.theme match {
      case m: Bootstrap4RenderThemeBase => grid_bootstrap4(ps)(f)
      case m: Bootstrap3RenderThemeBase => grid_bootstrap3(ps)(f)
      case m => grid_table(ps)(f)
    }

  protected def grid_bootstrap4[T](ps: List[T])(f: T => NodeSeq): Elem = {
    val g = strategy.gridContext
    val w = g.width
    val xscolumns = g.ncolumns.get(PhoneScreen) // < 576px
    val smcolumns = g.ncolumns.get(PhabletScreen) // >= 576px
    val mdcolumns = g.ncolumns.get(TabletScreen) // >= 768px
    val lgcolumns = g.ncolumns.get(LaptopScreen) // >= 992px
    val xlcolumns = g.ncolumns.get(DesktopScreen) // >= 1200px
    val colclass = Vector(
      xscolumns.map(x => s"col-${w / x}"),
      smcolumns.map(x => s"col-sm-${w / x}"),
      mdcolumns.map(x => s"col-md-${w / x}"),
      lgcolumns.map(x => s"col-lg-${w / x}"),
      xlcolumns.map(x => s"col-xl-${w / x}")
    ).flatten.mkString(" ")
    <div class="container"> { // container-fluid
      for (row <- ps.grouped(w)) yield {
        val rowclass = if (g.isNoGutters)
          "row no-gutters"
        else
          "row"
        <div class={rowclass}> {
          for (x <- row) yield {
            <div class={colclass}> {
              f(x)
            } </div>
          }
        } </div>
      }
    } </div>
  }

  protected def grid_bootstrap3[T](ps: List[T])(f: T => NodeSeq): Elem = {
    val g = strategy.gridContext
    val w = g.width
    val xscolumns = g.ncolumns.get(PhoneScreen) // < 768px
    val smcolumns = g.ncolumns.get(PhabletScreen) // >= 768px
    val mdcolumns = g.ncolumns.get(TabletScreen) // >= 992px
    val lgcolumns = g.ncolumns.get(LaptopScreen) orElse g.ncolumns.get(DesktopScreen) // >= 1200px
    val colclass = Vector(
      xscolumns.map(x => s"col-xs-${w / x}"),
      smcolumns.map(x => s"col-sm-${w / x}"),
      mdcolumns.map(x => s"col-md-${w / x}"),
      lgcolumns.map(x => s"col-lg-${w / x}")
    ).flatten.mkString(" ")
    <div class="container"> { // container-fluid
      for (row <- ps.grouped(w)) yield {
        val rowclass = if (g.isNoGutters)
          "row no-gutters"
        else
          "row"
        <div class={rowclass}> {
          for (x <- row) yield {
            <div class={colclass}> {
              f(x)
            } </div>
          }
        } </div>
      }
    } </div>
  }

  protected def grid_table[T](ps: List[T])(f: T => NodeSeq): Elem = {
    val g = strategy.gridContext
    val ncolumns = g.ncolumns.get(strategy.renderContext.screenKind) getOrElse {
      g.defaultNColumns
    }
    <div class={theme_grid.css.table.container}>{
      <table class={theme_grid.css.table.table}>
        <tbody class={theme_grid.css.table.tbody}>{
          for (row <- ps.grouped(ncolumns)) yield {
            <tr class={theme_grid.css.table.tbodyTr}>{
              for (x <- row) yield f(x)
            }</tr>
          }
        }</tbody>
      </table>
    }</div>
  }

  protected def grid_div[T](ps: List[T])(f: T => NodeSeq): Elem = {
    val g = strategy.gridContext
    val ncolumns = g.ncolumns.get(strategy.renderContext.screenKind) getOrElse {
      g.defaultNColumns
    }
    val r = <div class={theme_grid.css.div.container}>{
      <div class={theme_grid.css.div.row}>{
        for (row <- ps.grouped(ncolumns)) yield {
          <div class={theme_grid.css.div.field}>{
            for (x <- row) yield f(x)
          }</div>
        }
      }</div>
    }</div>
    XmlUtils.adjustEmptyDiv(r)
  }
}
