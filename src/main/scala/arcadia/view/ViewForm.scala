package arcadia.view

import scala.language.dynamics
import arcadia.ScenarioCommand
import arcadia.model._

/*
 * @since   Mar. 21, 2020
 * @version Apr. 18, 2020
 * @author  ASAMI, Tomoharu
 */
case class ViewForm(model: IFormModel, strategy: RenderStrategy) extends Dynamic {
  def locale = strategy.locale
  def action: String = model.action.toString
  def method: String = model.method.name
  def scenario: String = _get(ScenarioCommand.PROP_SCENARIO)
  def selectDynamic(name: String): String = _get(name)

  private def _get(name: String): String = model.get(name).map(strategy.format) getOrElse ""

  def isError: Boolean = model.conclusion.isError
  def isGeneralError: Boolean = model.conclusion.isGeneralError
  def generalError: String = getGeneralError getOrElse ""
  def getGeneralError: Option[String] = model.conclusion.getGeneralErrorMessage(locale)
  def isGeneralWarning: Boolean = model.conclusion.isGeneralWarning
  def generalWarning: String = getGeneralWarning getOrElse ""
  def getGeneralWarning: Option[String] = model.conclusion.getGeneralWarningMessage(locale)
  def isError(name: String): Boolean = model.conclusion.isError(name)
  def getError(name: String): Option[String] = model.conclusion.getErrorMessage(locale, name)
  def error(name: String): String = getError(name) getOrElse ""
  def isWarning(name: String): Boolean = model.conclusion.isWarning(name)
  def getWarning(name: String): Option[String] = model.conclusion.getWarningMessage(locale, name)
  def warning(name: String): String = getWarning(name) getOrElse ""
}
object ViewForm {
  def undefined(strategy: RenderStrategy): ViewForm = ViewForm(UndefinedFormModel, strategy)
}
