package arcadia.model

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

/*
 * @since   Oct. 28, 2017
 * @version Oct. 31, 2017
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class ParticleSpec extends WordSpec with Matchers with GivenWhenThen {
  "particle" should {
    "particle" which {
      "picture" in {
        val rule = """
[{
  "type": "picture",
  "src": "http://...",
  "alt": "One",
  "href": "http://..."
},{
  "type": "picture",
  "src": "http://...",
  "alt": "Two",
  "href": "http://..."
},{
  "type": "picture",
  "src": "http://...",
  "alt": "Two",
  "href": "http://..."
}]
"""
        Picture.parseList(rule)
      }
      "card" in {
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
        Card.parseList(rule)
      }
    }
  }
}
