package arcadia.view

import scala.util.control.NonFatal
import java.net.URI
import org.goldenport.record.v2._
import org.goldenport.xml.XmlUtils
import org.goldenport.exception.RAISE
import arcadia.domain._

/*
 * @since   Jul.  9, 2017
 *  version Aug. 30, 2017
 *  version Aug. 30, 2017
 * @version Sep.  4, 2017
 * @author  ASAMI, Tomoharu
 */
case class ViewObject(v: DomainObject) {
  def id: DomainObjectId = v.id
//  def status_id: Int = v.status_id
  def status = RAISE.notImplementedYetDefect
  def name: String = v.name
  def title: String = _normalize(v.title getOrElse "No title") // TODO
  def content: String = _normalize(v.content getOrElse "Under construction") // TODO
  def content__summary: String = _normalize(v.content getOrElse "Under construction") // TODO
  def image__icon: URI = v.imageIcon getOrElse {
    id.s match {
      case "pal-palshop-1377836961310-apparelcloud.blog-742c614e-bedd-4a18-ace3-447433b93f9f" => new URI("http://static.everforth.com/img/post/f9388dcf-c955-4b34-9d47-90b1c46652c9/555f88bd68dc86bb7242dc45ebca69cb.jpg")
      case "pal-palshop-1410983251862-apparelcloud.blog-1e1b8dcf-d662-4598-bd6d-d5ce72500755" => new URI("http://img2.everforth.com/img/post/a716b139-3103-4139-b811-9989a3f68323/cc70ef0af69016c2102ae2c994954eba.jpg")
      case "pal-palshop-1378000449352-apparelcloud.blog-37e2f626-2090-4f0f-ba76-535493d1d310" => new URI("http://static.everforth.com/img/post/721be228-200a-442d-87c0-772df0a1099f/28597bc04f81960bf3f0af0285174ccd.jpg")
      case "pal-palshop-1377947431680-apparelcloud.blog-7a93aa55-e683-4dae-b15a-38a376f7cca2" => new URI("http://static.everforth.com/img/post/ce9075dc-291b-4866-8ccb-983dc1f8ac1a/1bfac9cc725c14d3cb520497886735a1.jpg")
      case "pal-palshop-1377946471327-apparelcloud.blog-d6cc560e-f0a4-429b-aaaa-245a3614a206" => new URI("http://static.everforth.com/img/post/b94d14f8-b70e-4837-92b9-ad071b5fe411/aef1de088b806580985e4bd726973a30.jpg")
      case _ => new URI("http://livedoor.blogimg.jp/yurukuyaru/imgs/f/d/fd6a2365.jpg") // TODO
    }
  }
  def image__primary: URI = v.imagePrimary getOrElse ??? // {new URI("http://livedoor.blogimg.jp/yurukuyaru/imgs/f/d/fd6a2365.jpg")} // TODO

  private def _normalize(s: String): String = try {
    XmlUtils.toSummary(s)
  } catch {
    case NonFatal(e) => s
  }

  // def created_at: Timestamp = _typeclass.created_at(obj)
  // def updated_at: Timestamp = _typeclass.updated_at(obj)
  // def public_at: Option[Timestamp] = _typeclass.public_at(obj)
  // def close_at: Option[Timestamp] = _typeclass.close_at(obj)
  // def start_at: Option[Timestamp] = _typeclass.start_at(obj)
  // def end_at: Option[Timestamp] = _typeclass.end_at(obj)
  // def authors: Option[String] = _typeclass.authors(obj)
  // def contributers: Option[String] = _typeclass.contributers(obj)
  // def rights: Option[String] = _typeclass.rights(obj)
  // def source: Option[String] = _typeclass.source(obj)
  // def description: Option[String] = _typeclass.description(obj)
  // def mimetype: Option[String] = _typeclass.mimetype(obj)
  // def content: Option[String] = _typeclass.content(obj)
  // def content_mobile: Option[String] = _typeclass.content_mobile(obj)
  // def extension: Option[String] = _typeclass.extension(obj)
  // def locale: String = _typeclass.locale(obj)
  // def timezone: String = _typeclass.timezone(obj)
  // def language: Option[String] = _typeclass.language(obj)
  // def url: Option[String] = _typeclass.url(obj)
  // def image: Option[String] = _typeclass.image(obj)
  // def latitude: Option[Double] = _typeclass.latitude(obj)
  // def longitude: Option[Double] = _typeclass.longitude(obj)
  // def vertical_accuracy: Option[Double] = _typeclass.vertical_accuracy(obj)
  // def horizontal_accuracy: Option[Double] = _typeclass.horizontal_accuracy(obj)
  // def geohash: Option[String] = _typeclass.geohash(obj)
  // def location_id: Option[String] = _typeclass.location_id(obj)
  // def memo: Option[String] = _typeclass.memo(obj)
  // def externalid: Option[String] = _typeclass.externalid(obj)
  // def properties: Option[String] = _typeclass.properties(obj)
  // def rights_owner_read: Boolean = _typeclass.rights_owner_read(obj)
  // def rights_owner_write: Boolean = _typeclass.rights_owner_write(obj)
  // def rights_group_read: Boolean = _typeclass.rights_group_read(obj)
  // def rights_group_write: Boolean = _typeclass.rights_group_write(obj)
  // def rights_other_read: Boolean = _typeclass.rights_other_read(obj)
  // def rights_other_write: Boolean = _typeclass.rights_other_write(obj)
  // def shortid: Option[String] = _typeclass.shortid(obj)
  // def v1id: Option[Long] = _typeclass.v1id(obj)
  // def version: Option[String] = _typeclass.version(obj)
  // def context_id: Option[EFId] = _typeclass.context_id(obj)
  // def image_primary_id: Option[EFId] = _typeclass.image_primary_id(obj)
  // def link_primary_id: Option[EFId] = _typeclass.link_primary_id(obj)
  // def publish_at: Timestamp = _typeclass.publish_at(obj)
  // def display_level: Option[Int] = _typeclass.display_level(obj)
  // def name_en: Option[String] = _typeclass.name_en(obj)
  // def name_locale: Option[String] = _typeclass.name_locale(obj)
  // def revision: Option[Long] = _typeclass.revision(obj)
  // def predicate: Option[String] = _typeclass.predicate(obj)
  // def context_appid: Option[String] = _typeclass.context_appid(obj)
  // def aux_string_1: Option[String] = _typeclass.aux_string_1(obj)
  // def aux_string_2: Option[String] = _typeclass.aux_string_2(obj)
  // def aux_string_3: Option[String] = _typeclass.aux_string_3(obj)
  // def aux_string_4: Option[String] = _typeclass.aux_string_4(obj)
  // def aux_string_5: Option[String] = _typeclass.aux_string_5(obj)
  // def aux_string_6: Option[String] = _typeclass.aux_string_6(obj)
  // def aux_string_7: Option[String] = _typeclass.aux_string_7(obj)
  // def aux_string_8: Option[String] = _typeclass.aux_string_8(obj)
  // def aux_string_9: Option[String] = _typeclass.aux_string_9(obj)
  // def aux_string_10: Option[String] = _typeclass.aux_string_10(obj)
  // def aux_string_11: Option[String] = _typeclass.aux_string_11(obj)
  // def aux_string_12: Option[String] = _typeclass.aux_string_12(obj)
  // def aux_string_13: Option[String] = _typeclass.aux_string_13(obj)
  // def aux_string_14: Option[String] = _typeclass.aux_string_14(obj)
  // def aux_string_15: Option[String] = _typeclass.aux_string_15(obj)
  // def aux_string_16: Option[String] = _typeclass.aux_string_16(obj)
  // def aux_string_17: Option[String] = _typeclass.aux_string_17(obj)
  // def aux_string_18: Option[String] = _typeclass.aux_string_18(obj)
  // def aux_string_19: Option[String] = _typeclass.aux_string_19(obj)
  // def aux_string_20: Option[String] = _typeclass.aux_string_20(obj)
  // def aux_long_1: Option[Long] = _typeclass.aux_long_1(obj)
  // def aux_long_2: Option[Long] = _typeclass.aux_long_2(obj)
  // def aux_long_3: Option[Long] = _typeclass.aux_long_3(obj)
  // def aux_long_4: Option[Long] = _typeclass.aux_long_4(obj)
  // def aux_long_5: Option[Long] = _typeclass.aux_long_5(obj)
  // def aux_long_6: Option[Long] = _typeclass.aux_long_6(obj)
  // def aux_long_7: Option[Long] = _typeclass.aux_long_7(obj)
  // def aux_long_8: Option[Long] = _typeclass.aux_long_8(obj)
  // def aux_long_9: Option[Long] = _typeclass.aux_long_9(obj)
  // def aux_long_10: Option[Long] = _typeclass.aux_long_10(obj)
  // def aux_double_1: Option[Double] = _typeclass.aux_double_1(obj)
  // def aux_double_2: Option[Double] = _typeclass.aux_double_2(obj)
  // def aux_double_3: Option[Double] = _typeclass.aux_double_3(obj)
  // def aux_double_4: Option[Double] = _typeclass.aux_double_4(obj)
  // def aux_double_5: Option[Double] = _typeclass.aux_double_5(obj)
  // def aux_double_6: Option[Double] = _typeclass.aux_double_6(obj)
  // def aux_double_7: Option[Double] = _typeclass.aux_double_7(obj)
  // def aux_double_8: Option[Double] = _typeclass.aux_double_8(obj)
  // def aux_double_9: Option[Double] = _typeclass.aux_double_9(obj)
  // def aux_double_10: Option[Double] = _typeclass.aux_double_10(obj)
  // def aux_text_1: Option[String] = _typeclass.aux_text_1(obj)
  // def aux_text_2: Option[String] = _typeclass.aux_text_2(obj)
  // def aux_text_3: Option[String] = _typeclass.aux_text_3(obj)
  // def aux_text_4: Option[String] = _typeclass.aux_text_4(obj)
  // def aux_text_5: Option[String] = _typeclass.aux_text_5(obj)
  // def aux_text_6: Option[String] = _typeclass.aux_text_6(obj)
  // def aux_text_7: Option[String] = _typeclass.aux_text_7(obj)
  // def aux_text_8: Option[String] = _typeclass.aux_text_8(obj)
  // def aux_text_9: Option[String] = _typeclass.aux_text_9(obj)
  // def aux_text_10: Option[String] = _typeclass.aux_text_10(obj)
  // def appid: Option[AppId] = _typeclass.appid(obj)
  // def group_key_1: Option[Long] = _typeclass.group_key_1(obj)
  // def group_key_2: Option[Long] = _typeclass.group_key_2(obj)
  // def group_key_3: Option[Long] = _typeclass.group_key_3(obj)
  // def company_id: Option[Long] = _typeclass.company_id(obj)
  // def brand_id: Option[Long] = _typeclass.brand_id(obj)
  // def shop_id: Option[Long] = _typeclass.shop_id(obj)
  // def content_message: Option[String] = _typeclass.content_message(obj)
  // def content_push: Option[String] = _typeclass.content_push(obj)
  // def content_email: Option[String] = _typeclass.content_email(obj)
  // final def content_email_plain: Option[String] = _typeclass.content_email_plain(obj)
  // final def content_email_html: Option[String] = _typeclass.content_email_html(obj)
  // def associations: Option[String] = _typeclass.associations(obj)
  // def class_id: EFId = _typeclass.class_id(obj)
  // def created_by: EFId = _typeclass.created_by(obj)
  // def updated_by: EFId = _typeclass.updated_by(obj)
  // def owner_id: EFId = _typeclass.owner_id(obj)
  // def group_id: EFId = _typeclass.group_id(obj)
  // def privilege_id: Option[EFId] = _typeclass.privilege_id(obj)
  // def freezed: Int = _typeclass.freezed(obj)
  // // Image
  // def image_size: Option[Long] = _typeclass.image_size(obj)
  // def image_height: Option[Int] = _typeclass.image_height(obj)
  // def image_width: Option[Int] = _typeclass.image_width(obj)
  // def image_kind: Option[Int] = _typeclass.image_kind(obj)
  // //
  // def getCompanyId: Option[EFAppCompanyId] = _typeclass.getCompanyId(obj)
  // def getBrandId: Option[EFAppBrandId] = _typeclass.getBrandId(obj)
  // def getShopId: Option[EFAppShopId] = _typeclass.getShopId(obj)
}

object ViewObject {
  val empty = {
    val rec = Record.dataAppS(
      KEY_OBJECT_ID -> DomainObjectId.undefined
    )
    create(rec)
  }

  def create(rec: Record): ViewObject = ViewObject(RecordDomainObject(rec))
}
