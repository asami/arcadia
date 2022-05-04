package arcadia.view

import org.joda.time._
import org.goldenport.context.FormatContext
import arcadia.context.ExecutionContext

/*
 * @since   May.  2, 2022
 * @version May.  3, 2022
 * @author  ASAMI, Tomoharu
 */
case class FormatterContext(
  formatContext: FormatContext = FormatContext.default
) {
  def formatDateTime(p: Any): String = p match {
    case m: DateTime => formatContext.formatDateTime(m)
    case m: Long => formatContext.formatDateTime(m)
  }
  // def formatDateTime(p: DateTime): String = formatContext.formatDateTime(p)
  // def formatDateTime(p: Long): String = formatContext.formatDateTime(p)
  def formatDate(p: Any): String = p match {
    case m: DateTime => formatContext.formatDate(m)
    case m: Long => formatContext.formatDate(m)
  }
  // def formatDate(p: DateTime): String = formatContext.formatDate(p)
  // def formatDate(p: Long): String = formatContext.formatDate(p)
  def formatTime(p: Any): String = p match {
    case m: DateTime => formatContext.formatTime(m)
    case m: Long => formatContext.formatTime(m)
  }
  // def formatTime(p: DateTime): String = formatContext.formatTime(p)
  // def formatTime(p: Long): String = formatContext.formatTime(p)
}

object FormatterContext {
  val default = FormatterContext()

  def create(p: ExecutionContext): FormatterContext =
    FormatterContext(FormatContext.create(p.locale, p.timezone))
}
