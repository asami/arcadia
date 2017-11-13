package arcadia.view.tag

import scalaz.{Node => _, _}, Scalaz._
import scala.xml._
import org.goldenport.Strings
import org.goldenport.exception.RAISE
import org.goldenport.xml.XmlUtils
import org.goldenport.trace.Result
import arcadia._
import arcadia.context._
import arcadia.view._
import arcadia.model._

/*
 * @since   Oct. 11, 2017
 * @version Nov. 14, 2017
 * @author  ASAMI, Tomoharu
 */
trait Tag {
  lazy val show: String = s"${getClass.getSimpleName}${show_info}"
  protected def show_info: String =
    if (Strings.blankp(show_Info))
      ""
    else
      s"($show_Info)"
  protected def show_Info: String = ""

  final def eval(p: Expression): Option[XmlContent] =
    execute_Eval(p)
    // p.parcel.executeWithTraceOption(s"${show}#eval", p.parcel.show) {
    //   val r = execute_Eval(p)
    //   r.map(x => Result(x, x.show))
    // }
  protected def execute_Eval(p: Expression): Option[XmlContent]
}

trait SelectByName { self: Tag =>
  def name: String
  protected def execute_Eval(p: Expression): Option[XmlContent] =
    if (p.isLabel(name))
      Some(eval_Expression(p))
    else
      None

  protected def eval_Expression(p: Expression): XmlContent
}

case object ErrorTag extends Tag with SelectByName {
  val name = "error"

  protected def eval_Expression(p: Expression): XmlContent = {
    p.applyModel
  }
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
    val model = p.effectiveModel match {
      case m: ITableModel => m.withCaptionSchemaKind(caption, column, kind)
      case m => m
    }
    p.applyModel(model)
  }
}

case object GridTag extends Tag with SelectByName {
  val name = "grid"

  protected def eval_Expression(p: Expression): XmlContent = {
    val column = p.getStringList("column")
    val card = p.get("card")
    val model = p.effectiveModel match {
      case m: ITableModel => m.withSchemaKind(column, GridTable)
      case m => m
    }
    p.withTable(GridTable).withCard(card).applyModel(model)
  }
}

case object DetailTag extends Tag with SelectByName {
  val name = "detail"

  protected def eval_Expression(p: Expression): XmlContent = {
    // val column = p.getStringList("column")
    // val card = p.get("card")
    val model = p.effectiveModel match {
      case m: EntityDetailModel => m // .withSchemaKind(column, GridTable)
      case m => m
    }
//    p.withTable(GridTable).withCard(card).applyModel(model)
    p.applyModel(model)
  }
}

case object SearchBoxTag extends Tag with SelectByName {
  val name = "searchbox"

  protected def eval_Expression(p: Expression): XmlContent = {
    val columns = p.getStringList("column")
    // keywords
    // created_at
    // updated_at
    // publish_at
    // public_at
    // close_at
    // start_at
    // end_at
    val model = p.effectiveModel match {
      case m: SearchBoxModel => m // .withSchemaKind(column, GridTable)
      case m => m
    }
    // p.withTable(GridTable).withCard(card).applyModel(model)
    p.applyModel(model)
  }
}

case object ContentTag extends Tag with SelectByName {
  val name = "content"

  protected def eval_Expression(p: Expression): XmlContent = {
    val model = p.effectiveModel
    p.applyModel(model)
  }
}

case object BannerTag extends Tag with SelectByName {
  val name = "banner"

  protected def eval_Expression(p: Expression): XmlContent = {
    val model = p.effectiveModel
    p.applyModel(model)
  }
}

case object CarouselTag extends Tag with SelectByName {
  val name = "carousel"

  protected def eval_Expression(p: Expression): XmlContent = {
    val model = p.effectiveModel
    p.applyModel(model)
  }
}

case object BadgeTag extends Tag with SelectByName {
  val name = "badge"

  protected def eval_Expression(p: Expression): XmlContent = {
    val model = p.effectiveModel
    p.applyModel(model)
  }
}

case object NoticeTag extends Tag with SelectByName {
  val name = "notice"

  protected def eval_Expression(p: Expression): XmlContent = {
    val model = p.effectiveModel
    p.applyModel(model)
  }
}
