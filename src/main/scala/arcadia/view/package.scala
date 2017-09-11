package arcadia

import java.util.Locale

/*
 * @since   Aug.  2, 2017
 * @version Aug. 29, 2017
 * @author  ASAMI, Tomoharu
 */
package object view {
  val PlainHtml = RenderStrategy(Html, NormalSize, Locale.ENGLISH, None, PlainTheme, WebApplicationRule.empty, Partials.empty, None)
  val PlainSection = PlainHtml.copy(scope = Section)
  val PlainContent = PlainHtml.copy(scope = Content)
}
