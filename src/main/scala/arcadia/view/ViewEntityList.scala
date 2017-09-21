package arcadia.view

import arcadia.model._

/*
 * @since   Aug.  6, 2017
 * @version Sep. 20, 2017
 * @author  ASAMI, Tomoharu
 */

case class ViewEntityList(model: EntityListModel, strategy: RenderStrategy) {
  def first: ViewObject = apply(0)
  def second: ViewObject = apply(1)
  def third: ViewObject = apply(2)
  def fourth: ViewObject = apply(3)
  def fifth: ViewObject = apply(4)

  // 0 base
  def apply(i: Int): ViewObject = get(i) getOrElse ViewObject.empty
  def get(i: Int): Option[ViewObject] = model.records.lift(i).map(ViewObject.create)

  // 1 base
  def nth(i: Int): ViewObject = apply(i - 1)
  def getNth(i: Int): Option[ViewObject] = get(i - 1)
}
