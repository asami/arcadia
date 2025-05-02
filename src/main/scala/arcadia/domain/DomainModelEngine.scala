package arcadia.domain

import org.goldenport.exception.RAISE
import org.goldenport.trace.Result
import org.goldenport.context.Consequence
import org.goldenport.i18n.{I18NString, I18NElement}
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.record.v2.{Schema, Column}
import org.goldenport.record.v2.{XString, XInt}
import arcadia._
import arcadia.context.Query
import arcadia.model._

/*
 * @since   Dec.  4, 2022
 *  version Dec. 30, 2022
 *  version Jan.  1, 2023
 *  version Mar. 30, 2023
 *  version Apr. 16, 2023
 * @version Sep. 29, 2023
 * @author  ASAMI, Tomoharu
 */
class DomainModelEngine(
  model: DomainModel
) {
  def getEntitySchema(entitytype: DomainEntityType): Option[Schema] = model.getEntitySchema(entitytype)

  def getEntity(
    entitytype: DomainEntityType,
    id: DomainObjectId
  ): Consequence[Option[EntityDetailModel]] = Consequence.run {
    val caption = I18NElement(entitytype.name)
    // val schema = Schema(
    //   List(
    //     Column("id", XInt),
    //     Column("name", XString),
    //     Column("price", XInt)
    //   )
    // )
    // val record = Record.data(
    //   "id" -> 2,
    //   "name" -> "Orange",
    //   "price" -> 350
    // )
    // Some(EntityDetailModel(Some(caption), entitytype, Some(schema), record))
    model.getEntity(entitytype, id)
  }

  def readEntityList(q: Query): Consequence[EntityListModel] = Consequence.run {
    val entitytype = q.entityType
    val caption = I18NElement(entitytype.name)
    // val schema = Schema(
    //   List(
    //     Column("id", XInt),
    //     Column("name", XString),
    //     Column("price", XInt)
    //   )
    // )
    // val records = List(
    //   Record.data(
    //     "id" -> 1,
    //     "name" -> "Apple",
    //     "price" -> 300
    //   ),
    //   Record.data(
    //     "id" -> 2,
    //     "name" -> "Orange",
    //     "price" -> 350
    //   ),
    //   Record.data(
    //     "id" -> 3,
    //     "name" -> "Peach",
    //     "price" -> 400
    //   )
    // )
    // val transfer = Transfer(0, 100, 1000, 3, None)
    // EntityListModel(Some(caption), entitytype, Some(schema), records, transfer)
    model.readEntityList(q)
  }

  def createEntity(
    klass: DomainEntityType,
    data: IRecord
  ): Consequence[DomainObjectId] = model.createEntity(klass, data)

  def updateEntity(
    klass: DomainEntityType,
    id: DomainObjectId,
    data: IRecord
  ): Consequence[Unit] = model.updateEntity(klass, id, data)

  def deleteEntity(
    klass: DomainEntityType,
    id: DomainObjectId
  ): Consequence[Unit] = model.deleteEntity(klass, id)
}

object DomainModelEngine {
  def create(p: DomainModel): DomainModelEngine = new DomainModelEngine(p)
}
