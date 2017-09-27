package arcadia.view

import org.goldenport.record.v2._
import arcadia.model._

/*
 * @since   Sep. 24, 2017
 * @version Sep. 26, 2017
 * @author  ASAMI, Tomoharu
 */
trait IViewTable {
  def model: ITableModel with Model
  def strategy: RenderStrategy
  def tableKind: TableKind = model.tableKind
  def render = TableView(model).render(strategy)
  lazy val schema: Schema = model.getSchema getOrElse {
    strategy.withEntityType(model.getEntityType).resolveSchema
  }
  lazy val thead: ViewTHead = ViewTHead.create(this)
  lazy val tbody: ViewTBody = ViewTBody.create(this)
}

case class ViewTable(model: ITableModel with Model, strategy: RenderStrategy) extends IViewTable {
}

case class ViewTHead(model: TableHeadModel, elements: List[ViewTHeadTr], strategy: RenderStrategy) {
  def render = THeadView(model).render(strategy)
  def foreach(p: ViewTHeadTr => Unit): Unit = elements.foreach(p)
}
object ViewTHead {
  def create(p: IViewTable): ViewTHead = {
    val model = p.model.thead
    val a = for (c <- p.schema.columns) yield {
      val hm = TableHeadRecordDataModel(ValueModel.create(c.name), p.tableKind)
      ViewTh(hm, c.label(p.strategy.locale), p.strategy)
    }
    val rm = TableHeadRecordModel(p.schema, p.tableKind)
    val tr = ViewTHeadTr(rm, a.toList, p.strategy)
    ViewTHead(model, List(tr), p.strategy)
  }
}

case class ViewTBody(model: TableBodyModel, elements: List[ViewTBodyTr], strategy: RenderStrategy) {
  def render = TBodyView(model).render(strategy)
  def foreach(p: ViewTBodyTr => Unit): Unit = elements.foreach(p)
}
object ViewTBody {
  def create(p: IViewTable): ViewTBody = {
    val model = p.model.tbody
    val a = for (x <- p.model.records) yield {
      val b = for (c <- p.schema.columns) yield {
        val dm = TableBodyRecordDataModel(ValueModel.create(c, x), p.tableKind)
        ViewTd(dm, p.strategy.format(c, x), p.strategy)
      }
      val rm = TableBodyRecordModel(Some(p.schema), x, p.tableKind)
      ViewTBodyTr(rm, b.toList, p.strategy)
    }
    ViewTBody(model, a, p.strategy)
  }
}

case class ViewTHeadTr(model: TableHeadRecordModel, elements: List[ViewTrElement], strategy: RenderStrategy) {
  def render = TrView(model).render(strategy)
  def foreach(p: ViewTrElement => Unit): Unit = elements.foreach(p)
}

case class ViewTBodyTr(model: TableBodyRecordModel, elements: List[ViewTrElement], strategy: RenderStrategy) {
  def render = TrView(model).render(strategy)
  def foreach(p: ViewTrElement => Unit): Unit = elements.foreach(p)
}

trait ViewTrElement {
}

case class ViewTh(model: TableHeadRecordDataModel, v: String, strategy: RenderStrategy) extends ViewTrElement {
  def render = ThView(model).render(strategy)
  override def toString(): String = v
}

case class ViewTd(model: TableBodyRecordDataModel, v: String, strategy: RenderStrategy) extends ViewTrElement {
  def render = TdView(model).render(strategy)
  override def toString(): String = v
}
