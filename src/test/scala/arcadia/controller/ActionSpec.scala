package arcadia.controller

import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner
import java.net.URI
import org.goldenport.i18n.{I18NString, I18NElement}
import arcadia.context._
import arcadia.domain.DomainEntityType

/*
 * @since   Oct. 28, 2017
 *  version Oct. 31, 2017
 *  version Nov.  6, 2017
 *  version Jan. 22, 2018
 *  version Mar. 14, 2018
 *  version May.  8, 2019
 * @version Aug. 11, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class ActionSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "action" should {
    "typical" which {
      "carousel" in {
        Given("a carousel action rule with a source and sink")
        val rule = """{
  "action": "carousel",
  "source": "urn:prefer:free:/web/carousel",
  "sink": "carousel"
}"""

        When("the action rule is parsed")
        val actions = Action.parseActionList(rule)

        Then("a carousel action preserves its source and sink")
        actions should be(List(
          CarouselAction(
            Some(UrnSource("urn:prefer:free:/web/carousel")),
            Some(ModelHangerSink("carousel"))
          )
        ))
      }
      "notice" in {
        Given("a notice action rule with a source and sink")
        val rule = """{
  "action": "notice",
  "source": "urn:prefer:free:/web/notice",
  "sink": "notice"
}"""

        When("the action rule is parsed")
        val actions = Action.parseActionList(rule)

        Then("a notice action preserves its source and sink")
        actions should be(List(
          NoticeAction(
            Some(UrnSource("urn:prefer:free:/web/notice")),
            Some(ModelHangerSink("notice"))
          )
        ))
      }
      "content" in {
        Given("a content action rule with a source and sink")
        val rule = """{
  "action": "content",
  "source": "urn:prefer:free:/web/catchphrase",
  "sink": "catchphrase"
}"""

        When("the action rule is parsed")
        val actions = Action.parseActionList(rule)

        Then("a content action preserves its source and sink")
        actions should be(List(
          ContentAction(
            Some(UrnSource("urn:prefer:free:/web/catchphrase")),
            Some(ModelHangerSink("catchphrase"))
          )
        ))
      }
      "read-entity-list" in {
        Given("a read-entity-list action rule with an entity, source, and sink")
        val rule = """{
  "action": "read-entity-list",
  "entity": {"name": "productclass"},
  "source": "urn:prefer:free:/web/recommended_products",
  "sink": "recommended_products"
}"""

        When("the action rule is parsed")
        val actions = Action.parseActionList(rule)

        Then("the entity, source, and sink are preserved")
        actions should be(List(
          ReadEntityListAction(
            DomainEntityType("productclass"),
            None,
            None,
            None,
            Some(UrnSource("urn:prefer:free:/web/recommended_products")),
            Some(ModelHangerSink("recommended_products"))
          )
        ))
      }
      "list" in {
        Given("a rule containing the supported action variants")
        val rule = """[{
  "action": "carousel",
  "source": "urn:prefer:free:/web/carousel",
  "sink": "carousel"
},{
  "action": "banner",
  "source": "urn:prefer:free:/web/banner_brands",
  "sink": "banner_brands"
},{
  "action": "read-entity-list",
  "entity": {"name": "productclass"},
  "source": "urn:prefer:free:/web/recommended_products",
  "sink": "recommended_products"
},{
  "action": "read-entity-list",
  "entity": {"name": "productclass"},
  "query": {
    "limit": "20"
  },
  "sink": "ranking"
},{
  "action": "read-entity-list",
  "entity": {"name": "article"},
  "query": {
    "limit": "20"
  },
  "sink": "news_column"
},{
  "action": "banner",
  "source": "urn:prefer:free:/web/banner1",
  "sink": "banner1"
},{
  "action": "read-entity-list",
  "entity": {"name": "campaign"},
  "query": {
    "limit": "20"
  },
  "sink": "event"
},{
  "action": "banner",
  "source": "urn:prefer:free:/web/banner2",
  "sink": "banner2"
},{
  "action": "badge",
  "entity": "commerce_cart",
  "query": {
    "limit": "20"
  },
  "sink": "cart"
},{
  "action": "notice",
  "source": "urn:prefer:free:/web/notice",
  "sink": "notice"
},{
  "action": "content",
  "source": "urn:prefer:free:/web/catchphrase",
  "sink": "catchphrase"
}]
"""

        When("the action list is parsed")
        val actions = Action.parseActionList(rule)

        Then("each action variant preserves its configured semantics")
        actions should be(List(
          CarouselAction(
            Some(UrnSource("urn:prefer:free:/web/carousel")),
            Some(ModelHangerSink("carousel"))
          ),
          BannerAction(
            Some(UrnSource("urn:prefer:free:/web/banner_brands")),
            Some(ModelHangerSink("banner_brands"))
          ),
          ReadEntityListAction(
            DomainEntityType("productclass"),
            None,
            None,
            None,
            Some(UrnSource("urn:prefer:free:/web/recommended_products")),
            Some(ModelHangerSink("recommended_products"))
          ),
          ReadEntityListAction(
            DomainEntityType("productclass"),
            Some(Map("limit" -> "20")),
            None,
            None,
            None,
            Some(ModelHangerSink("ranking"))
          ),
          ReadEntityListAction(
            DomainEntityType("article"),
            Some(Map("limit" -> "20")),
            None,
            None,
            None,
            Some(ModelHangerSink("news_column"))
          ),
          BannerAction(
            Some(UrnSource("urn:prefer:free:/web/banner1")),
            Some(ModelHangerSink("banner1"))
          ),
          ReadEntityListAction(
            DomainEntityType("campaign"),
            Some(Map("limit" -> "20")),
            None,
            None,
            None,
            Some(ModelHangerSink("event"))
          ),
          BannerAction(
            Some(UrnSource("urn:prefer:free:/web/banner2")),
            Some(ModelHangerSink("banner2"))
          ),
          BadgeAction(
            Some("commerce_cart"),
            Some(Map("limit" -> "20")),
            None,
            Some(ModelHangerSink("cart"))
          ),
          NoticeAction(
            Some(UrnSource("urn:prefer:free:/web/notice")),
            Some(ModelHangerSink("notice"))
          ),
          ContentAction(
            Some(UrnSource("urn:prefer:free:/web/catchphrase")),
            Some(ModelHangerSink("catchphrase"))
          )
        ))
      }
      "directive" in {
        Given("an invoke-directive rule with URI, title, parameter, and sink")
        val rule = """{
  "action": "invoke-directive",
  "sink": "rcauser",
  "uri": "/rca/user",
  "title": "ユーザー",
  "parameters": [{
    "name": "id",
    "placeholder": "User ID/Access Token"
  }]
}"""

        When("the directive action rule is parsed")
        val actions = Action.parseActionList(rule)

        Then("the directive action preserves its configured semantics")
        actions should be(List(
          InvokeDirectiveAction(
            new URI("/rca/user"),
            None,
            Some(I18NElement("ユーザー")),
            None,
            None,
            Parameters.create(
              Parameter("id", placeholder = Some(I18NString("User ID/Access Token")))),
            None,
            Some(Sink("rcauser"))
          )
        ))
      }
     }
  }
}
