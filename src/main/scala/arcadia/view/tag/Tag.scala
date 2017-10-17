package arcadia.view.tag

import scalaz.{Node => _, _}, Scalaz._
import scala.xml._
import org.goldenport.exception.RAISE
import org.goldenport.xml.XmlUtils
import arcadia._
import arcadia.context._
import arcadia.view._
import arcadia.model._

/*
 * @since   Oct. 11, 2017
 * @version Oct. 16, 2017
 * @author  ASAMI, Tomoharu
 */
trait Tag {
  def eval(p: Expression): Option[XmlContent]
}

trait SelectByName { self: Tag =>
  def name: String
  def eval(p: Expression): Option[XmlContent] =
    if (p.isLabel(name))
      Some(eval_Expression(p))
    else
      None

  protected def eval_Expression(p: Expression): XmlContent
}

case object ModelTag extends Tag with SelectByName {
  val name = "model"

  protected def eval_Expression(p: Expression): XmlContent = {
    p.applyModel
  }
}

case object TableTag extends Tag with SelectByName {
  val name = "table"

  protected def eval_Expression(p: Expression): XmlContent = {
    val caption = p.get("caption")
    val column = p.getStringList("column")
    val kind = p.get("kind")
    val model = p.model match {
      case m: ITableModel => m.withCaptionSchemaKind(caption, column, kind)
    }
    p.applyModel(model)
  }
}

case object GridTag extends Tag with SelectByName {
  val name = "grid"

  protected def eval_Expression(p: Expression): XmlContent = {
    val column = p.getStringList("column")
    val model = p.model match {
      case m: ITableModel => m.withSchemaKind(column, GridTable)
    }
    p.applyModel(model)
  }
}
