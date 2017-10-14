package arcadia.view

import org.goldenport.exception.RAISE
import arcadia.model._

/*
 * @since   Aug.  6, 2017
 *  version Sep. 24, 2017
 * @version Oct.  9, 2017
 * @author  ASAMI, Tomoharu
 */
case class ViewEntityList(model: EntityListModel, strategy: RenderStrategy) extends IViewTable {
  def first: ViewEntity = apply(0)
  def second: ViewEntity = apply(1)
  def third: ViewEntity = apply(2)
  def fourth: ViewEntity = apply(3)
  def fifth: ViewEntity = apply(4)

  // 0 base
  def apply(i: Int): ViewEntity = get(i) getOrElse {
    RAISE.notImplementedYetDefect // TODO
  }
  def get(i: Int): Option[ViewEntity] = model.get(i).map(ViewEntity(_, strategy))

  // 1 base
  def nth(i: Int): ViewEntity = apply(i - 1)
  def getNth(i: Int): Option[ViewEntity] = get(i - 1)
}
