package arcadia.view.tag

import scalaz.{Node => _, _}, Scalaz._
import scala.xml._
import java.net.URI
import org.goldenport.Strings
import org.goldenport.exception.RAISE
import org.goldenport.xml.XmlUtils
import org.goldenport.i18n.{I18NString, I18NElement}
import org.goldenport.trace.Result
import org.goldenport.values.PathName
import arcadia._
import arcadia.context._
import arcadia.view._
import arcadia.model.{Model, ErrorModel, EmptyModel}

/*
 * @since   Sep. 30, 2017
 *  version Oct. 31, 2017
 *  version Nov. 14, 2017
 *  version Dec. 13, 2017
 *  version Jan. 21, 2018
 *  version Feb. 17, 2018
 *  version Apr. 15, 2018
 *  version May.  3, 2018
 * @version Aug.  5, 2018
 * @author  ASAMI, Tomoharu
 */
class TagEngine(
  tags: Tags
) {
  def call(parcel: Parcel): Call = Call(parcel)

  case class Call(parcel: Parcel) {
    // def apply(p: Content): Content = parcel.executeWithTrace(s"TagEngine#apply", p.show) {
    //   val r = p match {
    //     case m: XmlContent => _apply(m)
    //     case m => m
    //   }
    //   Result(r, r.show)
    // }

    def apply(p: Content): Content =
      p match {
        case m: XmlContent => _apply(m)
        case m => m
      }

    private def _apply(p: XmlContent): XmlContent = p.xml match {
      case m: Text => p
      case m: Elem =>
        val xs = m.child.flatMap(_eval_node)
        _eval_element(m, xs) getOrElse XmlContent(Group(Nil))
      case m: SpecialNode => p
      case Group(xs) => _group(xs.flatMap(_eval_node))
      case m: Document => _group(m.children.flatMap(_eval_node))
      case m if m.length > 0 => _group(m.toList.flatMap(_eval_node))
      case m => p
    }

    private def _group(ps: Seq[XmlContent]): XmlContent = XmlContent(ps)

    // private def _eval_content(p: Content): Option[XmlContent] = ???

    private def _eval_node(p: Node): Option[XmlContent] = p match {
      case m: Text => Some(XmlContent(p))
      case m: Elem =>
        val xs = m.child.flatMap(_eval_node)
        _eval_element(m, xs)
      case m: SpecialNode => Some(XmlContent(p))
      case Group(xs) => if (xs.isEmpty) None else Some(_group(xs.flatMap(_eval_node)))
      case m if m.length > 0 => Some(_group(m.toList.flatMap(_eval_node)))
      case m => Some(XmlContent(p))
    }

    private def _eval_element(p: Elem, children: Seq[XmlContent]): Option[XmlContent] = {
      val expr = Expression(_normalize(p), children, parcel)
      tags.stream.flatMap(_.eval(expr)).headOption orElse Some(XmlContent(expr.element))
    }

    private def _normalize(p: Elem) = p.copy(attributes = _normalize_attributes(p.attributes))

    private def _normalize_attributes(p: MetaData): MetaData = {
      val xs: Vector[(String, String)] = XmlUtils.attributeVector(p).flatMap(_normalize_attribute)
      XmlUtils.attributes(xs)
    }

    private def _normalize_attribute(p: (String, String)): Vector[(String, String)] = {
      val (name, value) = p
      if (true) // FUTURE
        Vector(p)
      else if (_is_upper(name))
        Vector((name.toLowerCase, _unescape(value)))
      else
        Vector(p)
    }

    private def _is_upper(p: String) = p.forall(_.isUpper)
    private def _unescape(p: String) = p.replace("&quot;", "\"").replace("&apos;", "\"")
  }
}
object TagEngine {
}

case class Tags(tags: Vector[Tag]) {
  lazy val stream = tags.toStream
  def complements(p: Tags): Tags = Tags(tags ++ p.tags)
  def complements(ps: List[Tags]): Tags = ps./:(this)(_ complements _)
}
object Tags {
  val empty = Tags(Vector.empty)
  val embeded = Tags(Vector(
    TableTag,
    GridTag,
    ListTag,
    DetailTag,
    SearchBoxTag,
    ContentTag,
    NoticeTag,
    BannerTag,
    CarouselTag,
    BadgeTag,
    ButtonTag,
    CommandTag,
    TabsTag,
    ModelTag,
    ErrorTag,
    WidgetTag
  ))
}

case class Expression(
  elem: Elem,
  children: Seq[XmlContent],
  parcel: Parcel
) {
  def prefix = elem.prefix
  def label = elem.label
  def tagName = Option(prefix).fold(label)(x => s"$x:$label")

  def get(key: String): Option[String] = XmlUtils.getAttribute(elem, key)
  def getStringList(key: String): Option[List[String]] = get(key).map(Strings.totokens)
  def take(key: String): String = get(key) getOrElse {
    RAISE.missingPropertyFault(key)
  }
  def take(key: String, d: String): String = get(key) | d

  def withTable(p: TableKind): Expression = copy(parcel = parcel.withTableKind(p))
  def withCard(p: Option[String]): Expression = p.fold(this) { x =>
    val card = CardKind.take(x)
    copy(parcel = parcel.withCardKindInGrid(card))
  }

  lazy val element = elem.copy(child = XmlUtils.seqOfNodeSeqToSeqOfNode(children.map(_.xml)))
  lazy val getModel: Option[Model] = parcel.model
  lazy val model: Model = getModel getOrElse {
    throw new IllegalStateException("TagEngine: No model")
  }
  lazy val strategy: RenderStrategy = parcel.render getOrElse {
    RAISE.noReachDefect
  }
  lazy val engine: ViewEngine = strategy.viewContext.map(_.engine) getOrElse {
    RAISE.noReachDefect
  }

  def show: String = XmlUtils.show(elem)

  // def model(key: Option[String]): Model = getModel(key) getOrElse {
  //   ErrorModel.create(parcel, s"No model for key '$key'.")
  // }

  // def getModel(key: Option[String]): Option[Model] =
  //   key.flatMap(parcel.getModel)

  def getModel(key: String): Option[Model] = parcel.getModel(key)

  def effectiveModel: Model = getEffectiveModel getOrElse {
    EmptyModel
  }

  def effectiveModelOrError: Model = getEffectiveModel getOrElse {
    ErrorModel.create(parcel, s"No model.")
  }

  def getEffectiveModel: Option[Model] = get("source") match {
    case Some(s) => getModel(s)
    case None => getModel
  }

  def applyModel: XmlContent = {
    val c = engine.applyComponentOption(parcel) getOrElse {
      model.apply(strategy.withScopeContent)
    }
    c.asXmlContent
  }

  def applyModel(p: Model): XmlContent = {
    val a = parcel.withModel(p)
    val c = engine.applyComponentOption(a) getOrElse {
      p.apply(strategy.withScopeContent)
    }
    c.asXmlContent
  }

  def isLabel(p: String): Boolean = prefix === "c" && label === p

  def format(p: I18NString): String = p(strategy.locale)

  def format(p: I18NElement): String = p(strategy.locale).text

  def resolveActionPathName(p: URI): PathName = {
    val pn = PathName(p.toString)
    if (pn.isAbsolute)
      parcel.context.fold(pn)(_.resolvePathName(pn))
    else
      pn
  }
}
