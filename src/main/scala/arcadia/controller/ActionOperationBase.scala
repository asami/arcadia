package arcadia.controller

import org.goldenport.record.v2.Record
// import com.everforth.everforth.context._
// import com.everforth.everforth.service.ServiceBase
// import com.everforth.everforth.entity.EverforthClassKind
import arcadia.model._
// import com.everforth.everforth.businesslogic.RecordFormattingLogic
// import com.everforth.everforth.repository.EverforthRepositoryEntry

/*
 * @since   Aug.  5, 2017
 * @version Aug. 29, 2017
 * @author  ASAMI, Tomoharu
 */
trait ActionOperationBase { // extends ServiceBase {
  val isDemo: Boolean = false

  // final def resource_list_model(
  //   c: EverforthClassKind,
  //   query: QueryContext
  // ): ResourceListModel = {
  //   val q = if (isDemo) query.withLimit(100, 100) else query
  //   val r = read_resource_grid(c, q)
  //   val caption = None
  //   val resource = c.resourceNameValue
  //   val schema = None
  //   val formatting = QueryFormatting.empty
  //   val records = r.data.map { x =>
  //     val entry = x.asInstanceOf[EverforthRepositoryEntry[_]]
  //     RecordFormattingLogic.defaultEntryToRecord(entry, formatting)
  //   }
  //   val transfer = r.transfer // TODO for demo
  //   val rs = records.filter(_is_available).toList
  //   ResourceListModel(
  //     caption,
  //     resource,
  //     schema,
  //     rs,
  //     transfer
  //   )
  // }

  // private def _is_available(p: Record): Boolean = if (!isDemo) true else {
  //   p.getRecordList('images).nonEmpty
  // }
}
