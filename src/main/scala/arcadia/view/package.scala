package arcadia

import java.util.Locale

/*
 * @since   Aug.  2, 2017
 * @version Sep. 14, 2017
 * @author  ASAMI, Tomoharu
 */
package object view {
  val PlainHtml = RenderStrategy(
    Html,
    NormalSize,
    Locale.ENGLISH,
    None,
    PlainTheme,
    StandardTable,
    WebApplicationRule.empty, Partials.empty, Components.empty,
    None
  )
  val PlainSection = PlainHtml.copy(scope = Section)
  val PlainContent = PlainHtml.copy(scope = Content)
}
