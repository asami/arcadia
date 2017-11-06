package arcadia.controller

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

/*
 * @since   Oct. 28, 2017
 *  version Oct. 31, 2017
 * @version Nov.  6, 2017
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class ActionSpec extends WordSpec with Matchers with GivenWhenThen {
  "a" should {
    "a" which {
      "carousel" in {
        val rule = """{
  "action": "carousel",
  "source": "urn:prefer:free:/web/carousel",
  "sink": "carousel"
}"""
        Action.parseActionList(rule) should be(List(
          CarouselAction(
            Some(UrnSource("urn:prefer:free:/web/carousel")),
            Some(ModelHangerSink("carousel"))
          )
        ))
      }
      "notice" in {
        val rule = """{
  "action": "notice",
  "source": "urn:prefer:free:/web/notice",
  "sink": "notice"
}"""
        Action.parseActionList(rule) should be(List(
          NoticeAction(
            Some(UrnSource("urn:prefer:free:/web/notice")),
            Some(ModelHangerSink("notice"))
          )
        ))
      }
      "content" in {
        val rule = """{
  "action": "content",
  "source": "urn:prefer:free:/web/catchphrase",
  "sink": "catchphrase"
}"""
        Action.parseActionList(rule) should be(List(
          ContentAction(
            Some(UrnSource("urn:prefer:free:/web/catchphrase")),
            Some(ModelHangerSink("catchphrase"))
          )
        ))
      }
      "read-entity-list" in {
        val rule = """{
  "action": "read-entity-list",
  "entity": "productclass",
  "source": "urn:prefer:free:/web/recommended_products",
  "sink": "recommended_products"
}"""
        Action.parseActionList(rule) should be(List(
          ReadEntityListAction(
            "productclass",
            None,
            None,
            None,
            Some(UrnSource("urn:prefer:free:/web/recommended_products")),
            Some(ModelHangerSink("recommended_products"))
          )
        ))
      }
      "list" in {
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
  "entity": "productclass",
  "source": "urn:prefer:free:/web/recommended_products",
  "sink": "recommended_products"
},{
  "action": "read-entity-list",
  "entity": "productclass",
  "query": {
    "limit": "20"
  },
  "sink": "ranking"
},{
  "action": "read-entity-list",
  "entity": "article",
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
  "entity": "campaign",
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
        Action.parseActionList(rule) should be(List(
          CarouselAction(
            Some(UrnSource("urn:prefer:free:/web/carousel")),
            Some(ModelHangerSink("carousel"))
          ),
          BannerAction(
            Some(UrnSource("urn:prefer:free:/web/banner_brands")),
            Some(ModelHangerSink("banner_brands"))
          ),
          ReadEntityListAction(
            "productclass",
            None,
            None,
            None,
            Some(UrnSource("urn:prefer:free:/web/recommended_products")),
            Some(ModelHangerSink("recommended_products"))
          ),
          ReadEntityListAction(
            "productclass",
            Some(Map("limit" -> "20")),
            None,
            None,
            None,
            Some(ModelHangerSink("ranking"))
          ),
          ReadEntityListAction(
            "article",
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
            "campaign",
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
//       "list-x" in {
//         val rule = """[{
//   "action": "carousel",
//   "source": "urn:prefer:free:/web/carousel",
//   "sink": "carousel"
// },{
//   "action": "banner",
//   "source": "urn:prefer:free:/web/banner_brands",
//   "sink": "banner_brands"
// },{
//   "action": "read-entity-list",
//   "entity": "productclass",
//   "source": "urn:prefer:free:/web/recommended_products",
//   "sink": "recommended_products"
// },{
//   "action": "read-entity-list",
//   "entity": "productclass",
//   "query": {
//     "limit": "20"
//   },
//   "sink": "ranking"
// },{
//   "action": "read-entity-list",
//   "entity": "article",
//   "query": {
//     "limit": "20"
//   },
//   "sink": "news_column"
// },{
//   "action": "banner",
//   "source": "urn:prefer:free:/web/banner1",
//   "sink": "banner1"
// },{
//   "action": "read-entity-list",
//   "entity": "campaign",
//   "query": {
//     "limit": "20"
//   },
//   "sink": "event"
// }]
// """
//         Action.parseActionList(rule) should be(List(
//           CarouselAction(
//             Some(UrnSource("urn:prefer:free:/web/carousel")),
//             Some(ModelHangerSink("carousel"))
//           ),
//           BannerAction(
//             Some(UrnSource("urn:prefer:free:/web/banner_brands")),
//             Some(ModelHangerSink("banner_brands"))
//           ),
//           ReadEntityListAction(
//             "productclass",
//             None,
//             None,
//             Some(UrnSource("urn:prefer:free:/web/recommended_products")),
//             Some(ModelHangerSink("recommended_products"))
//           ),
//           ReadEntityListAction(
//             "productclass",
//             Some(Map("limit" -> "20")),
//             None,
//             None,
//             Some(ModelHangerSink("ranking"))
//           ),
//           ReadEntityListAction(
//             "article",
//             Some(Map("limit" -> "20")),
//             None,
//             None,
//             Some(ModelHangerSink("news_column"))
//           ),
//           BannerAction(
//             Some(UrnSource("urn:prefer:free:/web/banner1")),
//             Some(ModelHangerSink("banner1"))
//           ),
//           ReadEntityListAction(
//             "campaign",
//             Some(Map("limit" -> "20")),
//             None,
//             None,
//             Some(ModelHangerSink("event"))
//           )
//         ))
//       }
     }
  }
}
