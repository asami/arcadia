package arcadia.view.tag

import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner
import scala.xml.XML
import arcadia.XmlContent
import arcadia.view.ViewEngine
import arcadia.view.expression.ExpressionEngine

/*
 * @since   Aug. 11, 2026
 * @version Aug. 11, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class OptionalTagSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "optional tag" should {
    "render children only for exact nonblank bindings" which {
      "omit the complete subtree when no listed key is present" in {
        Given("an optional subtree with no matching bindings")
        val xml = """<root xmlns:c="urn:arcadia"><c:optional binding="one two"><span>hidden</span></c:optional><p>kept</p></root>"""

        When("the embedded tags are evaluated")
        val output = _render(xml, Map.empty).toHtmlString

        Then("the optional subtree is absent while ordinary content remains")
        output should not include ("hidden")
        output should not include ("optional")
        output should include ("<p>kept</p>")
      }

      "retain one matching subtree without its c:optional wrapper" in {
        Given("one of two exact bindings with a nonblank value")
        val xml = """<root xmlns:c="urn:arcadia"><c:optional binding="one two"><span id="kept">kept</span><p>${locale == '/ja' ? '動画を見る' : 'Watch video'}</p></c:optional></root>"""
        val bindings = Map[String, AnyRef]("two" -> " value ", "locale" -> "/ja")

        When("the tag output is passed through the expression engine")
        val tagged = _render(xml, bindings)
        val output = new ExpressionEngine().apply(null, ViewEngine.Bindings(bindings), tagged).asXmlContent.toHtmlString

        Then("the retained children are present, the wrapper is stripped, and the locale expression resolves")
        output should include ("id=\"kept\"")
        output should include ("kept")
        output should include ("動画を見る")
        output should not include ("c:optional")
        output should not include ("${locale")
      }

      "retain independently wrapped children when both listed keys are present" in {
        Given("two optional subtrees whose listed bindings are all present")
        val xml = """<root xmlns:c="urn:arcadia"><c:optional binding="one two"><i>first</i></c:optional><c:optional binding="three four"><b>second</b></c:optional></root>"""
        val bindings = Map[String, AnyRef]("one" -> "1", "two" -> "2", "three" -> "3", "four" -> "4")

        When("the embedded tags are evaluated")
        val output = _render(xml, bindings).toHtmlString

        Then("both child groups remain and neither optional wrapper remains")
        output should include ("<i>first</i>")
        output should include ("<b>second</b>")
        output should not include ("optional")
      }

      "treat null and blank string values as absent and avoid dotted traversal" in {
        Given("null, blank, and nested-looking bindings without exact keys")
        val xml = """<root xmlns:c="urn:arcadia"><c:optional binding="null-key blank-key notice.media.infographic.public_path"><span>unexpected</span></c:optional></root>"""
        val nested = Map[String, AnyRef]("media" -> Map("infographic" -> "nested").asInstanceOf[AnyRef]).asInstanceOf[AnyRef]
        val bindings = Map[String, AnyRef](
          "null-key" -> null,
          "blank-key" -> " \t ",
          "notice" -> nested
        )

        When("the embedded tags are evaluated")
        val output = _render(xml, bindings).toHtmlString

        Then("only exact nonblank flat keys can retain the subtree")
        output should not include ("unexpected")
        output should not include ("optional")
      }
    }
  }

  private def _render(xml: String, values: Map[String, AnyRef]): XmlContent =
    new TagEngine(Tags.embeded).
      call(null, ViewEngine.Bindings(values)).
      apply(XmlContent(XML.loadString(xml))).
      asXmlContent
}
