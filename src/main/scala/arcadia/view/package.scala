package arcadia

import java.util.Locale

/*
 * @since   Aug.  2, 2017
 *  version Sep. 25, 2017
 *  version Oct. 21, 2017
 * @version Apr.  2, 2025
 * @author  ASAMI, Tomoharu
 */
package object view {
  val PlainHtml = RenderStrategy(
    Locale.ENGLISH,
    PlainTheme,
    SchemaRule.empty,
    WebApplicationRule.empty, Partials.empty, Components.empty,
    None,
    RenderContext.empty,
    None
  )
  val PlainSection = PlainHtml.withScopeSection
  val PlainContent = PlainHtml.withScopeContent
}
