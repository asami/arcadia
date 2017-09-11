package arcadia.controller

import org.goldenport.values.ResourceName
import arcadia._
import arcadia.context._
import arcadia.model._
// import com.everforth.everforth.entity.{EverforthClassKind, NewsClass, BlogClass}
import org.goldenport.i18n.{I18NString, I18NElement}

/*
 * @since   Jul. 16, 2017
 *  version Aug. 29, 2017
 * @version Sep.  4, 2017
 * @author  ASAMI, Tomoharu
 */
trait Action {
  def apply(parcel: Parcel): Parcel
}

case class IndexAction(
) extends Action {
  import IndexAction._
  def apply(parcel: Parcel): Parcel =
    if (parcel.model.isDefined) parcel else {
      val pagename = I18NString("Index page name") // TODO
      val headline = I18NElement("Index headline") // TODO
      val resources = parcel.context.map(TakeResources(_, parcel).apply) getOrElse Nil
      val carousel = None
      val model = IndexModel(
        Some(pagename),
        Some(headline),
        resources,
        carousel
      )
      parcel.withModel(model)
    }
}

object IndexAction {
  case class TakeResources(context: ExecutionContext, parcel: Parcel) extends ActionOperationBase {
    override val isDemo = false

    def apply: List[(String, ResourceListModel)] = {
      List(
        _read_resource_list_news,
        _read_resource_list_blog
      )
    }

    private def _read_resource_list_news() = {
      val rsc = ResourceName("news")
      val q = Query(rsc, 0, 10, 20)
      rsc.v -> context.readResourceList(q)
    }

    private def _read_resource_list_blog() = {
      val rsc = ResourceName("blog")
      val q = Query(rsc, 0, 10, 20)
      rsc.v -> context.readResourceList(q)
    }

//     private def _read_resource_grid(c: EverforthClassKind) = {
//       val q = QueryContext.create(c, 0, 10, 20) // TODO
//       val m = resource_list_model(c, q)
//       (c.resourceName, m)
//     }

//     private def _read_resource_grid_demo(c: EverforthClassKind) = {
// //      val q = QueryContext.create(c, 0, 10, 20).withBrands(Vector(252)) // TODO
//       val q = QueryContext.create(c, 0, 10, 20).withIdsBag(Vector(
//         "pal-palshop-1377836961310-apparelcloud.blog-742c614e-bedd-4a18-ace3-447433b93f9f",
//         "pal-palshop-1410983251862-apparelcloud.blog-1e1b8dcf-d662-4598-bd6d-d5ce72500755",
//         "pal-palshop-1378000449352-apparelcloud.blog-37e2f626-2090-4f0f-ba76-535493d1d310",
//         "pal-palshop-1377947431680-apparelcloud.blog-7a93aa55-e683-4dae-b15a-38a376f7cca2",
//         "pal-palshop-1377946471327-apparelcloud.blog-d6cc560e-f0a4-429b-aaaa-245a3614a206"
//       )) // TODO
//       val m = resource_list_model(c, q)
//       (c.resourceName, m)
//     }
  }
}
