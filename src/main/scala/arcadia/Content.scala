package arcadia

import scalaz.{Node => _, _}
import Scalaz._

import scala.util.control.NonFatal
import scala.xml._
import scala.concurrent.duration._
import java.net.URI
import java.net.URL
import java.io.OutputStream
import org.joda.time.DateTime
import org.goldenport.exception.RAISE
import org.goldenport.context.Conclusion
import org.goldenport.bag.{Bag, ChunkBag}
import org.goldenport.io.IoUtils
import org.goldenport.record.v2.Record
import org.goldenport.xml.{XmlPrinter, XmlUtils}
import org.goldenport.xml.XhtmlUtils
import org.goldenport.util.SeqUtils.mkStringOption
import org.goldenport.util.DateTimeUtils.httpDateTimeString
import arcadia.model.ErrorModel
import arcadia.context.Session

/*
 * @since   Jul. 16, 2017
 *  version Aug. 30, 2017
 *  version Sep. 30, 2017
 *  version Oct. 27, 2017
 *  version Nov. 17, 2017
 *  version Dec. 21, 2017
 *  version Jan.  8, 2018
 *  version Apr. 20, 2020
 *  version Feb. 27, 2022
 *  version Mar. 28, 2022
 *  version May. 28, 2022
 * @version Mar. 21, 2025
 * @author  ASAMI, Tomoharu
 */
sealed trait Content {
  def code: Int
  def mimetype: MimeType
  def contenttype: String = mimetype.name
  def charset: Option[String]
  def expiresKind: Option[ExpiresKind]
  def expiresPeriod: Option[FiniteDuration]
  def proxyExpiresPeriod: Option[FiniteDuration]
  def etag: Option[ETag]
  def lastModified: Option[DateTime]
  def maxAge: Option[FiniteDuration] = expiresPeriod orElse expiresKind.flatMap(_.cachePeriod)
  def sMaxage: Option[FiniteDuration] = proxyExpiresPeriod orElse expiresKind.flatMap(_.proxyCachePeriod)
  def httpHeader: Seq[(String, String)] = {
    def cachecontrol: Option[String] = {
      val a: Vector[String] = expiresKind.map(_.cacheControlHeader).getOrElse(Vector.empty)
      val b: Vector[String] = Vector(
        maxAge.map(_.toSeconds).map(x => s"max-age=$x"),
        sMaxage.map(_.toSeconds).map(x => s"s-maxage=$x")
      ).flatten
      mkStringOption(a ++ b, ",")
    }
    def expires = maxAge.map { x =>
      httpDateTimeString(System.currentTimeMillis + x.toMillis)
    }
    def vary = mkStringOption(expiresKind.map(_.varyHeader), ", ")
    val r = Record.dataAppOption(
      "Cache-Control" -> cachecontrol,
      "Expires" -> expires,
      "ETag" -> etag.map(_.v),
      "LastModified" -> lastModified.map(x => httpDateTimeString(x)),
      "Vary" -> vary
    )
    r.toStringVector
  }
  def session: Option[Session]
  def asXml: NodeSeq = RAISE.noReachDefect
  def asXmlContent: XmlContent = RAISE.noReachDefect

  def withCode(code: Int): Content
  def withExpiresPeriod(p: FiniteDuration): Content

  def write(out: OutputStream): Unit
  def writeClose(out: OutputStream): Unit = try {
    write(out)
  } finally {
    out.close()
  }

  protected final def write_string(out: OutputStream, p: String): Unit =
    charset.map(IoUtils.write(out, p, _)).getOrElse(IoUtils.write(out, p))

  def addCallTree(p: String): Content = this

  def show: String

  protected final def to_contenttype(charset: Option[String]): String =
    charset.fold(mimetype.name)(to_contenttype(_))

  protected final def to_contenttype(charset: String): String = 
    s"${mimetype.name}; charset=$charset"

  protected final def to_contenttype_utf8 = to_contenttype("UTF-8")
}
object Content {
}

case class StringContent(
  mimetype: MimeType,
  charset: Option[String],
  string: String,
  expiresKind: Option[ExpiresKind],
  expiresPeriod: Option[FiniteDuration],
  proxyExpiresPeriod: Option[FiniteDuration],
  etag: Option[ETag],
  lastModified: Option[DateTime],
  code: Int = 200
) extends Content {
  val session = None
  def withCode(p: Int) = copy(code = p)
  def withExpiresPeriod(p: FiniteDuration): StringContent = copy(expiresPeriod = Some(p))

  lazy val show = "StringContent"

  override def contenttype = to_contenttype(charset)
  override def asXml: NodeSeq = try {
    XmlUtils.parseNodeSeq(string)
  } catch {
    case NonFatal(e) => Text(string)
  }
  override def asXmlContent = XmlContent(
    mimetype,
    asXml,
    expiresKind,
    expiresPeriod,
    proxyExpiresPeriod,
    etag,
    lastModified,
    code
  )

  def write(out: OutputStream): Unit = write_string(out, string)

  override def addCallTree(p: String) = copy(string = string + "\n===== Call Tree (StringContent) =====\n" + p)
}
object StringContent {
  def apply(s: String): StringContent =
    StringContent(MimeType.text_html, None, s, None, None, None, None, None)
  def apply(s: String, expireskind: ExpiresKind): StringContent =
    StringContent(MimeType.text_html, None, s, Some(expireskind), None, None, None, None)

  def apply(mime: MimeType, s: String): StringContent =
    StringContent(mime, None, s, None, None, None, None, None)
}

case class XmlContent(
  mimetype: MimeType,
  xml: NodeSeq,
  expiresKind: Option[ExpiresKind],
  expiresPeriod: Option[FiniteDuration],
  proxyExpiresPeriod: Option[FiniteDuration],
  etag: Option[ETag],
  lastModified: Option[DateTime],
  code: Int = 200
) extends Content {
  val session = None
  val charset = Some("utf-8") // TODO
  override def asXml: NodeSeq = xml
  override def asXmlContent: XmlContent = this
  override lazy val contenttype = to_contenttype(charset)

  lazy val toHtmlString: String = XmlPrinter.html(xml)

  def withCode(p: Int) = copy(code = p)
  def withXml(xml: NodeSeq) = copy(xml = xml)
  def withExpiresPeriod(p: FiniteDuration): XmlContent = copy(expiresPeriod = Some(p))

  lazy val show = "XmlContent"

  def +(rhs: XmlContent): XmlContent = {
    import ExpiresKind.Implicits._
    val ek: Option[ExpiresKind] = expiresKind |+| rhs.expiresKind
    copy(xml = XmlUtils.concat(xml, rhs.xml), expiresKind = ek)
  }

  def write(out: OutputStream): Unit = write_string(out, toHtmlString)

  override def addCallTree(p: String) = copy(xml = XmlUtils.concat(
    xml,
    <div><h4>===== Call Tree (XmlContent) ====</h4><pre>{p}</pre></div>
  ))

  def addScriptElement(p: Elem): XmlContent = {
    def go(x: NodeSeq): Node = x match {
      case m: Elem =>
        if (m.label == "html")
          f(m)
        else
          m
      case Group(ms) => Group(ms.map(go))
      case m: Node => m
      case m: NodeSeq => Group(m)
    }
    def f(x: Elem): Elem = x.copy(child = x.child :+ p).asInstanceOf[Elem]
    xml match {
      case m: Elem =>
        if (m.label == "html")
          copy(xml = f(m))
        else
          this
      case Group(ms) => copy(xml = Group(ms.map(go)))
      case m: Node => this
      case m => this
    }
  }
}
object XmlContent {
  val empty = apply(Group(Nil))

  def apply(xml: NodeSeq): XmlContent = XmlContent(MimeType.text_html, xml, None, None, None, None, None)
  def apply(xml: NodeSeq, expires: ExpiresKind): XmlContent = XmlContent(MimeType.text_html, xml, Some(expires), None, None, None, None)
  def apply(xml: NodeSeq, expires: Option[ExpiresKind]): XmlContent = XmlContent(MimeType.text_html, xml, expires, None, None, None, None)
  def apply(xml: NodeSeq, expires: Option[ExpiresKind], code: Int): XmlContent = XmlContent(MimeType.text_html, xml, expires, None, None, None, None, code)

  def apply(ps: Seq[XmlContent]): XmlContent = {
    ps.toList match {
      case Nil => XmlContent.empty
      case x :: Nil => x
      case x :: xs => xs./:(x)(_ + _)
    }
  }

  def text(p: String): XmlContent = XmlContent(MimeType.text_html, Text(p), None, None, None, None, None)

  def staticPage(xml: NodeSeq) = XmlContent(MimeType.text_html, xml, Some(StaticPageExpires), None, None, None, None)
  def stablePage(xml: NodeSeq) = XmlContent(MimeType.text_html, xml, Some(StablePageExpires), None, None, None, None)
  def agilePage(xml: NodeSeq) = XmlContent(MimeType.text_html, xml, Some(AgilePageExpires), None, None, None, None)
  def fragilePage(xml: NodeSeq) = XmlContent(MimeType.text_html, xml, Some(FragilePageExpires), None, None, None, None)
  def dynamicPage(xml: NodeSeq) = XmlContent(MimeType.text_html, xml, Some(DynamicPageExpires), None, None, None, None)
  def privatePage(xml: NodeSeq) = XmlContent(MimeType.text_html, xml, Some(PrivatePageExpires), None, None, None, None)
  def noCachePage(xml: NodeSeq) = XmlContent(MimeType.text_html, xml, Some(NoCacheExpires), None, None, None, None)

  def load(mime: MimeType, url: URL): XmlContent = {
    // XmlContent(mime, XML.load(url), None, None, None, None, None) // TODO parse encoding
    val xml = mime match {
      case m if m.isXml => XML.load(url)
      case m if m.isHtml => XhtmlUtils.loadHtmlFragmentNode(url)
      case m => Text(IoUtils.toText(url))
    }
    staticPage(xml)
  }

  def loadHtml(url: URL): XmlContent = load(MimeType.text_html, url)
}

case class BinaryContent(
  mimetype: MimeType,
  binary: ChunkBag,
  expiresKind: Option[ExpiresKind],
  expiresPeriod: Option[FiniteDuration],
  proxyExpiresPeriod: Option[FiniteDuration],
  etag: Option[ETag],
  lastModified: Option[DateTime],
  code: Int = 200
) extends Content {
  val session = None
  val charset = None

  def withCode(p: Int) = copy(code = p)
  def withExpiresPeriod(p: FiniteDuration): BinaryContent = copy(expiresPeriod = Some(p))

  def write(out: OutputStream): Unit = binary.copyTo(out)

  lazy val show = "BinaryContent"
}
object BinaryContent {
  def apply(mimetype: MimeType, binary: ChunkBag, expireskind: ExpiresKind): BinaryContent =
    BinaryContent(mimetype, binary, Some(expireskind), None, None, None, None)
}

case class RedirectContent(
  uri: URI = new URI("index.html"),
  session: Option[Session] = None,
  code: Int = 303
) extends Content {
  def mimetype: MimeType = MimeType.text_html
  val charset = None
  lazy val xml: NodeSeq = Group(Nil)
  def expiresKind: Option[ExpiresKind] = Some(NoCacheExpires)
  def expiresPeriod: Option[FiniteDuration] = None
  def proxyExpiresPeriod: Option[FiniteDuration] = None
  def etag: Option[ETag] = None
  def lastModified: Option[DateTime] = None
  def getAccessToken: Option[String] = session.flatMap(_.accessToken)

  def withExpiresPeriod(p: FiniteDuration): RedirectContent = this
  def withCode(p: Int) = copy(code = p)

  lazy val show = "RedirectContent"

  def write(out: OutputStream): Unit = write_string(out, show)
}
object RedirectContent {
  def apply(p: String): RedirectContent = RedirectContent(new URI(p))
  def apply(p: String, session: Option[Session]): RedirectContent = RedirectContent(new URI(p), session)
}

trait ErrorContent extends Content {
  def code: Int
  def mimetype: MimeType = MimeType.text_html
  lazy val xml: NodeSeq = Group(Nil)
  def expiresKind: Option[ExpiresKind] = Some(NoCacheExpires)
  def expiresPeriod: Option[FiniteDuration] = None
  def proxyExpiresPeriod: Option[FiniteDuration] = None
  def etag: Option[ETag] = None
  def lastModified: Option[DateTime] = None
  def withExpiresPeriod(p: FiniteDuration): Content = this
  def withCode(p: Int) = this
  def session = None
  def show: String

  def write(out: OutputStream): Unit = write_string(out, show)
}

case class ErrorModelContent(model: ErrorModel) extends ErrorContent {
  def code = model.code
  val charset = None
  lazy val show = s"ErrorModelContent: $code"
}

case class NotFoundContent(pathname: String) extends ErrorContent {
  def code = 404
  val charset = None
  lazy val show = s"NotFoundContent: $pathname"
}

case class ExceptionContent(e: Throwable) extends ErrorContent {
  val conclusion = Conclusion.make(e)
  val code = conclusion.code
  val charset = None
  lazy val show = s"ExceptionContent: ${conclusion.message}"

  override def asXml: NodeSeq = throw e
  override def asXmlContent: XmlContent = throw e
}

// https://stackoverflow.com/questions/18148884/difference-between-no-cache-and-must-revalidate
// https://qiita.com/karore/items/2dc6ab8347c940ea4648
sealed trait ExpiresKind {
  // private, no-store, no-cache, must-revalidate, proxy-revalidate
  def cacheControlHeader: Vector[String]
  // User-Agent, Accept-Encoding, Accept-Language, Cookie
  def varyHeader: Vector[String]
  def cachePeriod: Option[FiniteDuration]
  def proxyCachePeriod: Option[FiniteDuration]
  def +(rhs: ExpiresKind): ExpiresKind
}
object ExpiresKind {
  val default: ExpiresKind = StablePageExpires

  object Implicits {
    implicit lazy val ExpiresKindMonoid: Monoid[ExpiresKind] = new Monoid[ExpiresKind] {
      def append(lhs: ExpiresKind, rhs: => ExpiresKind): ExpiresKind = lhs + rhs
      def zero: ExpiresKind = default
    }
  }
}
case object AssetsExpires extends ExpiresKind { // CDN
  val cacheControlHeader = Vector("public")
  val varyHeader = Vector.empty
  val cachePeriod = Some(1.day)
  val proxyCachePeriod = None
  def +(rhs: ExpiresKind): ExpiresKind = rhs
}
case object StaticPageExpires extends ExpiresKind { // CDN
  val cacheControlHeader = Vector("public")
  val varyHeader = Vector.empty
  val cachePeriod = Some(6.hours)
  val proxyCachePeriod = None
  def +(rhs: ExpiresKind) = rhs match {
    case AssetsExpires => this
    case _ => rhs
  }
}
case object StablePageExpires extends ExpiresKind { // CDN
  val cacheControlHeader = Vector("public")
  val varyHeader = Vector.empty
  val cachePeriod = Some(3.hours)
  val proxyCachePeriod = None
  def +(rhs: ExpiresKind) = rhs match {
    case AssetsExpires => this
    case StaticPageExpires => this
    case _ => rhs
  }
}
case object AgilePageExpires extends ExpiresKind { // CDN
  val cacheControlHeader = Vector("public") // must-revalidate
  val varyHeader = Vector.empty
  val cachePeriod = Some(1.hours)
  val proxyCachePeriod = None
  def +(rhs: ExpiresKind) = rhs match {
    case AssetsExpires => this
    case StaticPageExpires => this
    case StablePageExpires => this
    case _ => rhs
  }
}
case object FragilePageExpires extends ExpiresKind { // CDN
  val cacheControlHeader = Vector("public") // "no-cache"
  val varyHeader = Vector.empty
  val cachePeriod = Some(30.minutes)
  val proxyCachePeriod = None
  def +(rhs: ExpiresKind) = rhs match {
    case AssetsExpires => this
    case StaticPageExpires => this
    case StablePageExpires => this
    case AgilePageExpires => this
    case _ => rhs
  }
}
case object DynamicPageExpires extends ExpiresKind { // CDN
  val cacheControlHeader = Vector("public")
  val varyHeader = Vector.empty
  val cachePeriod = Some(5.minutes)
  val proxyCachePeriod = None
  def +(rhs: ExpiresKind) = rhs match {
    case PrivatePageExpires => rhs
    case NoCacheExpires => rhs
    case _ => this
  }
}
case object PrivatePageExpires extends ExpiresKind {
  val cacheControlHeader = Vector("private")
  val varyHeader = Vector("Cookie")
  def cachePeriod = FragilePageExpires.cachePeriod
  val proxyCachePeriod = None
  def +(rhs: ExpiresKind) = rhs match {
    case NoCacheExpires => rhs
    case _ => this
  }
}
case object NoCacheExpires extends ExpiresKind {
  val cacheControlHeader = Vector("private", "no-store", "no-cache", "must-revalidate")
  val varyHeader = Vector.empty
  val cachePeriod = None
  val proxyCachePeriod = None
  def +(rhs: ExpiresKind) = this
}

case class ETag(v: String) extends AnyVal
