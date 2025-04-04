package arcadia.view.expression

import scalaz.{Node => _, _}, Scalaz._
import scala.util.control.NonFatal
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
 * @since   Mar. 21, 2025
 *  version Mar. 21, 2025
 * @version Apr.  1, 2025
 * @author  ASAMI, Tomoharu
 */
class ExpressionEngine(
) {
  def apply(parcel: Parcel, bindings: ViewEngine.Bindings, content: Content): Content =
    call(parcel, bindings).apply(content)

  def call(parcel: Parcel, bindings: ViewEngine.Bindings): Call = Call(parcel, bindings)

  case class Call(
    parcel: Parcel,
    bindings: ViewEngine.Bindings
  ) extends CallBase(parcel, bindings) {
    override protected def eval_Attribute(p: (String, String)): Vector[(String, String)] = {
      val (name, value) = p
      val v = ViewEngine.evalExpression(value, bindings)
      Vector(name -> v)
    }
  }

  abstract class CallBase(
    parcel: Parcel,
    bindings: ViewEngine.Bindings
  ) {
    def apply(p: Content): Content = try {
      p match {
        case m: XmlContent => _apply(m).withCode(p.code)
        case m: ErrorContent => m
        case m => m
      }
    } catch {
      case NonFatal(e) => ExceptionContent(e)
    }

    private def _apply(p: XmlContent): XmlContent = p.xml match {
      case m: Text => p
      case m: Elem =>
        val xs = m.child.flatMap(eval_node)
        eval_element(m, xs) getOrElse XmlContent(Group(Nil))
      case m: SpecialNode => p
      case Group(xs) => to_xml_content(xs.flatMap(eval_node))
      case m: Document => to_xml_content(m.children.flatMap(eval_node))
      case m if m.length > 0 => to_xml_content(m.toList.flatMap(eval_node))
      case m => p
    }

    protected def to_xml_content(ps: Seq[XmlContent]): XmlContent = {
      val xs = ps.map(_apply)
      val mimetype = _mimetype(xs)
      val xml = _xml(xs)
      val expireskind = _expires_kind(xs)
      val expiresperiod = _expires_period(xs)
      val proxyexpiresperiod = _proxy_expires_period(xs)
      val etag = _etag(xs)
      val lastmodified = _last_modified(xs)
      XmlContent(mimetype, xml, expireskind, expiresperiod, proxyexpiresperiod, etag, lastmodified)
    }

    private def _mimetype(ps: Seq[XmlContent]) = ps.headOption.fold(MimeType.text_html)(_.mimetype)

    private def _xml(ps: Seq[XmlContent]): NodeSeq = XmlUtils.concat(ps.map(_.xml))

    private def _expires_kind(ps: Seq[XmlContent]) = ps.headOption.flatMap(_.expiresKind)

    private def _expires_period(ps: Seq[XmlContent]) = ps.headOption.flatMap(_.expiresPeriod)

    private def _proxy_expires_period(ps: Seq[XmlContent]) = ps.headOption.flatMap(_.proxyExpiresPeriod)

    private def _etag(ps: Seq[XmlContent]) = ps.headOption.flatMap(_.etag)

    private def _last_modified(ps: Seq[XmlContent]) = ps.headOption.flatMap(_.lastModified)

    // private def _eval_content(p: Content): Option[XmlContent] = ???

    protected def eval_node(p: Node): Option[XmlContent] = p match {
      case m: Text => Some(XmlContent(p))
      case m: Elem =>
        val xs = m.child.flatMap(eval_node)
        // println(s"${xs}")
        val r = eval_element(m, xs)
        // println(s"${m} => ${r}")
        r
      case m: SpecialNode => Some(XmlContent(p))
      case Group(xs) => if (xs.isEmpty) None else Some(to_xml_content(xs.flatMap(eval_node)))
      case m if m.length > 0 => Some(to_xml_content(m.toList.flatMap(eval_node)))
      case m => Some(XmlContent(p))
    }

    protected def eval_element(p: Elem, children: Seq[XmlContent]): Option[XmlContent] = {
      val elem = eval_attributes(p)
      eval_Element(elem, children)
    }

    protected def eval_Element(p: Elem, children: Seq[XmlContent]): Option[XmlContent] = {
      val xs = children.map(_.xml).map {
        case m: Node => m
        case m: NodeSeq => RAISE.noReachDefect("eval_Element")
      }
      Some(XmlContent(p.copy(child = xs)))
    }

    protected def eval_attributes(p: Elem): Elem = p.copy(attributes = eval_attributes(p.attributes))

    protected def eval_attributes(p: MetaData): MetaData = {
      val xs: Vector[(String, String)] = XmlUtils.attributeVector(p).flatMap(eval_attribute)
      XmlUtils.attributes(xs)
    }

    protected def eval_attribute(p: (String, String)): Vector[(String, String)] =
      eval_Attribute(p)

    protected def eval_Attribute(p: (String, String)): Vector[(String, String)] = {
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

object ExpressionEngine {
}
