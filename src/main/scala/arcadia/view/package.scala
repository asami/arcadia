package arcadia

import java.util.Locale

/*
 * @since   Aug.  2, 2017
 *  version Sep. 25, 2017
 * @version Oct. 16, 2017
 * @author  ASAMI, Tomoharu
 */
package object view {
  val PlainHtml = RenderStrategy(
    Html,
    NormalSize,
    Locale.ENGLISH,
    PlainTheme,
    SchemaRule.empty,
    WebApplicationRule.empty, Partials.empty, Components.empty,
    RenderContext.empty,
    None
  )
  val PlainSection = PlainHtml.copy(scope = Section)
  val PlainContent = PlainHtml.copy(scope = Content)
}
