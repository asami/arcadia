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
 * @version Dec. 30, 2022
 * @author  ASAMI, Tomoharu
 */
class DomainModelEngine(
  model: DomainModel
) {
  def getEntity(
    entitytype: DomainEntityType,
    id: DomainObjectId
  ): Consequence[Option[EntityDetailModel]] = Consequence {
    val caption = I18NElement(entitytype.v)
    val schema = Schema(
      List(
        Column("id", XInt),
        Column("name", XString),
        Column("price", XInt)
      )
    )
    val record = Record.data(
      "id" -> 2,
      "name" -> "Orange",
      "price" -> 350
    )
    Some(EntityDetailModel(Some(caption), entitytype, Some(schema), record))
  }

  def readEntityList(q: Query): Consequence[EntityListModel] = Consequence {
    val entitytype = q.entityType
    val caption = I18NElement(entitytype.v)
    val schema = Schema(
      List(
        Column("id", XInt),
        Column("name", XString),
        Column("price", XInt)
      )
    )
    val records = List(
      Record.data(
        "id" -> 1,
        "name" -> "Apple",
        "price" -> 300
      ),
      Record.data(
        "id" -> 2,
        "name" -> "Orange",
        "price" -> 350
      ),
      Record.data(
        "id" -> 3,
        "name" -> "Peach",
        "price" -> 400
      )
    )
    val transfer = Transfer(0, 100, 1000, 3, None)
    EntityListModel(Some(caption), entitytype, Some(schema), records, transfer)
  }

  def createEntity(
    klass: DomainEntityType,
    data: IRecord
  ): Consequence[DomainObjectId] = ???

  def updateEntity(
    klass: DomainEntityType,
    id: DomainObjectId,
    data: IRecord
  ): Consequence[Unit] = ???

  def deleteEntity(
    klass: DomainEntityType,
    id: DomainObjectId
  ): Consequence[Unit] = ???
}

object DomainModelEngine {
  def create(p: DomainModel): DomainModelEngine = new DomainModelEngine(p)
}
