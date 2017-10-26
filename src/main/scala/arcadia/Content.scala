package arcadia

import scalaz._, Scalaz._
import scala.xml._
import scala.concurrent.duration._
import java.net.URI
import org.joda.time.DateTime
import org.goldenport.exception.RAISE
import org.goldenport.bag.ChunkBag
import org.goldenport.record.v2.Record
import org.goldenport.xml.XmlUtils
import org.goldenport.util.SeqUtils.mkStringOption
import org.goldenport.util.DateTimeUtils.httpDateTimeString

/*
 * @since   Jul. 16, 2017
 *  version Aug. 30, 2017
 *  version Sep. 30, 2017
 * @version Oct. 24, 2017
 * @author  ASAMI, Tomoharu
 */
sealed trait Content {
  def mimetype: MimeType
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
  def asXml: NodeSeq = RAISE.noReachDefect
  def asXmlContent: XmlContent = RAISE.noReachDefect

  def withExpiresPeriod(p: FiniteDuration): Content
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
  lastModified: Option[DateTime]
) extends Content {
  def withExpiresPeriod(p: FiniteDuration): StringContent = copy(expiresPeriod = Some(p))
}
object StringContent {
  def apply(s: String): StringContent =
    StringContent(MimeType.text_html, None, s, None, None, None, None, None)
  def apply(s: String, expireskind: ExpiresKind): StringContent =
    StringContent(MimeType.text_html, None, s, Some(expireskind), None, None, None, None)
}

case class XmlContent(
  mimetype: MimeType,
  xml: NodeSeq,
  expiresKind: Option[ExpiresKind],
  expiresPeriod: Option[FiniteDuration],
  proxyExpiresPeriod: Option[FiniteDuration],
  etag: Option[ETag],
  lastModified: Option[DateTime]
) extends Content {
  override def asXml: NodeSeq = xml
  override def asXmlContent: XmlContent = this

  def withXml(xml: NodeSeq) = copy(xml = xml)
  def withExpiresPeriod(p: FiniteDuration): XmlContent = copy(expiresPeriod = Some(p))

  def +(rhs: XmlContent): XmlContent = {
    import ExpiresKind.Implicits._
    val ek: Option[ExpiresKind] = expiresKind |+| rhs.expiresKind
    copy(xml = XmlUtils.concat(xml, rhs.xml), expiresKind = ek)
  }
}
object XmlContent {
  val empty = apply(Group(Nil))

  def apply(xml: NodeSeq): XmlContent = XmlContent(MimeType.text_html, xml, None, None, None, None, None)
  def apply(xml: NodeSeq, expires: ExpiresKind): XmlContent = XmlContent(MimeType.text_html, xml, Some(expires), None, None, None, None)
  def apply(xml: NodeSeq, expires: Option[ExpiresKind]): XmlContent = XmlContent(MimeType.text_html, xml, expires, None, None, None, None)

  def apply(ps: Seq[XmlContent]): XmlContent = {
    ps.toList match {
      case Nil => XmlContent.empty
      case x :: Nil => x
      case x :: xs => xs./:(x)(_ + _)
    }
  }

  def staticPage(xml: NodeSeq) = XmlContent(MimeType.text_html, xml, Some(StaticPageExpires), None, None, None, None)
  def stablePage(xml: NodeSeq) = XmlContent(MimeType.text_html, xml, Some(StablePageExpires), None, None, None, None)
  def agilePage(xml: NodeSeq) = XmlContent(MimeType.text_html, xml, Some(AgilePageExpires), None, None, None, None)
  def fragilePage(xml: NodeSeq) = XmlContent(MimeType.text_html, xml, Some(FragilePageExpires), None, None, None, None)
  def dynamicPage(xml: NodeSeq) = XmlContent(MimeType.text_html, xml, Some(DynamicPageExpires), None, None, None, None)
  def privatePage(xml: NodeSeq) = XmlContent(MimeType.text_html, xml, Some(PrivatePageExpires), None, None, None, None)
  def noCachePage(xml: NodeSeq) = XmlContent(MimeType.text_html, xml, Some(NoCacheExpires), None, None, None, None)
}

case class BinaryContent(
  mimetype: MimeType,
  binary: ChunkBag,
  expiresKind: Option[ExpiresKind],
  expiresPeriod: Option[FiniteDuration],
  proxyExpiresPeriod: Option[FiniteDuration],
  etag: Option[ETag],
  lastModified: Option[DateTime]
) extends Content {
  def withExpiresPeriod(p: FiniteDuration): BinaryContent = copy(expiresPeriod = Some(p))
}
object BinaryContent {
  def apply(mimetype: MimeType, binary: ChunkBag, expireskind: ExpiresKind): BinaryContent =
    BinaryContent(mimetype, binary, Some(expireskind), None, None, None, None)
}

case class RedirectContent(
  uri: URI = new URI("index.html")
) extends Content {
  def mimetype: MimeType = MimeType.text_html
  lazy val xml: NodeSeq = Group(Nil)
  def expiresKind: Option[ExpiresKind] = Some(NoCacheExpires)
  def expiresPeriod: Option[FiniteDuration] = None
  def proxyExpiresPeriod: Option[FiniteDuration] = None
  def etag: Option[ETag] = None
  def lastModified: Option[DateTime] = None
  def withExpiresPeriod(p: FiniteDuration): RedirectContent = this
}
object RedirectContent {
  def apply(p: String): RedirectContent = RedirectContent(new URI(p))
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
  val cacheControlHeader = Vector("public", "no-cache")
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
  val cacheControlHeader = Vector("public", "no-cache", "must-revalidate")
  val varyHeader = Vector.empty
  val cachePeriod = Some(1.hours)
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
