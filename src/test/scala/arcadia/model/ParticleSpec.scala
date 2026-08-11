package arcadia.model

import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner
import java.net.URI
import org.goldenport.i18n.I18NString

/*
 * @since   Oct. 28, 2017
 * @version Aug. 11, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class ParticleSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "particle" should {
    "particle" which {
      "picture" in {
        Given("a URI picture with alternative text and a link")
        val src = new URI("http://example.com/image")
        val alt = I18NString("Example image")
        val href = new URI("http://example.com/image-detail")
        val picture = Picture.UriPicture(
          src,
          attributes = Picture.Attributes(alt = Some(alt), href = Some(href))
        )

        When("the picture properties are accessed")
        val actualsrc = picture.src
        val actualalt = picture.alt
        val actualhref = picture.href

        Then("the original source, alternative text, and link are preserved")
        actualsrc should be(src)
        actualalt should be(Some(alt))
        actualhref should be(Some(href))
      }
      "card" in {
        Given("a rule containing one card particle")
        val rule = """
{
  "type": "card",
  "image-top": {
    "src": "",
    "alt": ""
  },
  "title": "",
  "subtitle": "",
  "content": "",
  "href": "http://..."
}
"""

        When("the card particle rule is parsed")
        val particles = Card.parseList(rule)

        Then("the declared card particle is interpreted")
        particles should have size 1
      }
    }
  }
}
