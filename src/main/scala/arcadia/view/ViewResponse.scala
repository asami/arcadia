package arcadia.view

import org.goldenport.record.v2._
import arcadia.model._
import arcadia.context.Response

/*
 * @since   Sep. 27, 2017
 * @version Sep. 27, 2017
 * @author  ASAMI, Tomoharu
 */
case class ViewResponse(response: Response, strategy: RenderStrategy) {
  def render = response.render(strategy)
  def json = response.json

  def asPropertySheet = ViewModel(response.asPropertySheet, strategy)
  def asPropertyTable = ViewTable(response.asPropertyTable, strategy)
  def asEntityDetail = ViewModel(response.asEntityDetail, strategy)
  def asEntityList = ViewTable(response.asEntityList, strategy)
}
