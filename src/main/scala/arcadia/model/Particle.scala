package arcadia.model

import scala.xml.{NodeSeq, Group, Text, Node, Elem}
import java.net.{URI, URL}
import java.util.Locale
import play.api.libs.json._
import org.goldenport.Strings
import org.goldenport.exception.RAISE
import org.goldenport.i18n.{I18NString, I18NElement}
import org.goldenport.xml.{XmlUtils, XhtmlUtils}
import org.goldenport.value._
import org.goldenport.record.v2._
import org.goldenport.json.JsonUtils
import org.goldenport.util.SeqUtils
import com.asamioffice.goldenport.text.UString
import com.asamioffice.goldenport.io.UURL
import arcadia._
import arcadia.context.Query
import arcadia.scenario.Event
import arcadia.view.RenderContext
import arcadia.domain._

/*
 * @since   Oct. 29, 2017
 *  version Nov. 22, 2017
 *  version Dec. 17, 2017
 *  version Jan.  6, 2018
 *  version Apr.  8, 2018
 *  version May.  4, 2018
 *  version Jul. 23, 2018
 *  version Aug.  6, 2018
 * @version Oct. 24, 2018
 * @author  ASAMI, Tomoharu
 */
sealed trait Particle {
}

sealed trait Link extends Particle {
  def id: DomainObjectId
  def getDataHref(ctx: RenderContext): Option[URI]
  def dataHref(ctx: RenderContext): URI
}

case class DomainEntityLink(
  id: DomainEntityId,
  base_href: Option[URI]
) extends Link {
  def getDataHref(ctx: RenderContext): Option[URI] = Some(dataHref(ctx))
  def dataHref(ctx: RenderContext): URI = (
    base_href.map(d => ctx.uri(d, id)) orElse
      ctx.dataHref.map(d => ctx.uri(d, id)) getOrElse
      ctx.uri(id.entityType, id)
  )
}

case class DomainObjectLink(
  id: DomainObjectId,
  base_href: Option[URI]
) extends Link {
  def getDataHref(ctx: RenderContext): Option[URI] = (
    base_href.map(d => ctx.uri(d, id)) orElse
      ctx.dataHref.map(d => ctx.uri(d, id)) orElse
      id.getEntityType.map(d => ctx.uri(d, id))
  )
  def dataHref(ctx: RenderContext): URI = getDataHref(ctx) getOrElse {
    RAISE.noReachDefect
  }
}

case class TitleLine(
  title: Option[I18NElement],
  subtitle: Option[I18NElement]
) extends Particle {
  def toOption: Option[TitleLine] =
    if (title.isEmpty)
      None
    else
      Some(this)
}
object TitleLine {
  val blank = TitleLine(Some(I18NElement("")), None)
  def create(title: I18NElement): TitleLine = TitleLine(Some(title), None)
}

case class Picture( // HTML5 picture
  // TODO source
  // TODO map/area
  src: URI, // (1280)
  src_l: Option[URI], // 1280
  src_m: Option[URI], // 640
  src_s: Option[URI], // 320
  src_xs: Option[URI], // 160
  src_raw: Option[URI],
  alt: Option[I18NString],
  href: Option[URI],
  size: Option[Int],
  height: Option[Int],
  width: Option[Int],
  caption: Option[I18NElement],
  description: Option[I18NElement]
) extends Particle {
  // def srcString = src.toString
  lazy val large = src_l getOrElse src
  lazy val medium = src_m orElse src_l getOrElse src
  lazy val small = src_s orElse src_m orElse src_l getOrElse src
  lazy val extrasmall = src_xs orElse src_s orElse src_m orElse src_l getOrElse src
  lazy val img = src.toString
  lazy val l = large.toString
  lazy val m = medium.toString
  lazy val s = small.toString
  lazy val xs = extrasmall.toString
  // def altString(locale: Locale): String = alt(locale)
  def alt(locale: Locale): String = alt.map(_.as(locale)).getOrElse("")
}
object Picture {
  def create(src: URI): Picture = Picture(src, None, None, None, None, None, None, None, None, None, None, None, None)

  def create(src: URL): Picture = create(src.toURI)

  def create(src: String): Picture =
    if (src.startsWith("{"))
      Particle.parseParticle(src) match {
        case JsSuccess(x, _) => x match {
          case m: Picture => m
          case m => RAISE.syntaxErrorFault(m.toString)
        }
        case m: JsError => RAISE.syntaxErrorFault(m.toString)
      }
    else
      create(UURL.getURLFromFileOrURLName(src))

  def parseList(p: String): List[Picture] = Particle.parseParticleList(p) match {
    case JsSuccess(xs, _) => xs.collect {
      case m: Picture => m
    }
    case m: JsError => RAISE.syntaxErrorFault(m.toString)
  }

  def get(rec: Record, key: Symbol): Option[Picture] = rec.getOne(key).map {
    case m: Picture => m
    case m: URI => create(m)
    case m: URL => create(m)
    case m: String => create(m)
    case m => create(m.toString)
  }
}

case class Card(
  image_top: Option[Picture],
  header: Option[TitleLine],
  footer: Option[TitleLine],
  content: Option[I18NElement],
  summary: Option[I18NElement],
  link: Option[DomainEntityLink],
  record: Option[Record]
) extends Particle {
  def isImageTopOnly = image_top.isDefined && header.isEmpty && footer.isEmpty && content.isEmpty && summary.isEmpty
}
object Card {
  def create(pic: Picture, rec: Record): Card = Card(Some(pic), None, None, None, None, None, Some(rec))

  def create(pic: Picture, header: TitleLine, content: NodeSeq, rec: Record): Card = Card(Some(pic), Some(header), None, Some(I18NElement(content)), Some(_summary(content)), None, Some(rec))

  def create(pic: Picture, header: Option[TitleLine], content: NodeSeq, rec: Record): Card = Card(Some(pic), header, None, Some(I18NElement(content)), Some(_summary(content)), None, Some(rec))

  def create(pic: Picture, header: Option[TitleLine], content: Option[NodeSeq], rec: Record): Card = Card(Some(pic), header, None, content.map(I18NElement(_)), content.map(_summary), None, Some(rec))

  def create(pic: Picture, header: Option[TitleLine], content: Option[NodeSeq], summary: Option[NodeSeq], link: Option[DomainEntityLink], rec: Record): Card = Card(Some(pic), header, None, content.map(I18NElement(_)), summary.map(I18NElement(_)).orElse(content.map(_summary)), link, Some(rec))

  def create(pic: Option[Picture], header: Option[TitleLine], footer: Option[TitleLine], content: NodeSeq, summary: Option[NodeSeq]): Card = Card(pic, header, footer, Some(I18NElement(content)), summary.map(I18NElement(_)).orElse(Some(_summary(content))), None, None)

  def parseList(p: String): List[Card] = Particle.parseParticleList(p) match {
    case JsSuccess(xs, _) => xs.collect {
      case m: Card => m
    }
    case m: JsError => RAISE.syntaxErrorFault(m.toString)
  }

  private def _summary(p: NodeSeq): I18NElement = {
    val s = Strings.cutstring(p.text)
    I18NElement(s)
  }
}

sealed trait LabelIndicator extends NamedValueInstance {
  def name: String
}
object LabelIndicator extends EnumerationClass[LabelIndicator] {
  val elements = Vector(
    PrimaryLabelIndicator,
    SecondaryLabelIndicator,
    SuccessLabelIndicator,
    DangerLabelIndicator,
    WarningLabelIndicator,
    InfoLabelIndicator,
    LightLabelIndicator,
    DarkLabelIndicator
  )

  def parseJsValue(json: JsValue): JsResult[LabelIndicator] = json match {
    case JsString(s) => JsSuccess(get(s) getOrElse PrimaryLabelIndicator)
    case m => JsError(s"Unknown particle: $json")
  }
}
case object PrimaryLabelIndicator extends LabelIndicator {
  val name = "primary"
}
case object SecondaryLabelIndicator extends LabelIndicator {
  val name = "secondary"
}
case object SuccessLabelIndicator extends LabelIndicator {
  val name = "success"
}
case object DangerLabelIndicator extends LabelIndicator {
  val name = "danger"
}
case object WarningLabelIndicator extends LabelIndicator {
  val name = "warning"
}
case object InfoLabelIndicator extends LabelIndicator {
  val name = "info"
}
case object LightLabelIndicator extends LabelIndicator { // badge
  val name = "light"
}
case object DarkLabelIndicator extends LabelIndicator { // badge
  val name = "dark"
}

case class Badge(
  number: Option[Long],
  label: Option[String],
  indicator: Option[LabelIndicator]
) extends Particle {
  def asLabel: String = number.map(_.toString) orElse label getOrElse ""
  def asIndicatorName: String = indicator.map(_.name) getOrElse PrimaryLabelIndicator.name
}
object Badge {
  val empty = Badge(None, None, None)

  def create(number: Long): Badge = Badge(Some(number), None, None)

  def parse(p: String): Badge = {
    val r = Particle.BadgeFormat.reads(Json.parse(p))
    r match {
      case JsSuccess(s, _) => s
      case m: JsError => RAISE.syntaxErrorFault(m.toString)
    }
  }
}

case class Xml(
  i18n: Option[I18NElement],
  node: Option[NodeSeq]
) extends Particle {
  def apply(locale: Locale): NodeSeq = i18n.map(_.apply(locale)) orElse node getOrElse Group(Nil)
}
object Xml {
  val empty = Xml(None, None)

  def parse(p: String): Xml = {
    val s = p.trim
    if (s.startsWith("<"))
      Xml(None, Some(XhtmlUtils.parseFragmentNode(p)))
    else if (s.startsWith("{"))
      Xml(Some(I18NElement.parse(p)), None)
    else
      Xml(None, None)
  }
}

case class Submits(submits: Vector[Submit]) extends Particle {
}
object Submits {
  def apply(p: Submit, ps: Submit*): Submits = Submits(
    (p +: ps).toVector
  )
}

case class Submit(kind: SubmitKind, label: I18NString) extends Particle {
  def name = ScenarioCommand.PROP_SUBMIT_PREFIX + kind.name
  def value(locale: Locale) = label(locale)
}
object Submit {
  def apply(kind: SubmitKind): Submit = Submit(kind, kind.label)
}

sealed trait SubmitKind {
  def name: String
  def label: I18NString = I18NString(UString.capitalize(name))
}
case object OkSubmitKind extends SubmitKind {
  def name: String = Event.EVENT_OK
}
case object CancelSubmitKind extends SubmitKind {
  def name: String = Event.EVENT_CANCEL
}
case object InputSubmitKind extends SubmitKind {
  def name: String = Event.EVENT_INPUT
}
case object CreateSubmitKind extends SubmitKind {
  def name: String = Event.EVENT_CREATE
}
case object UpdateSubmitKind extends SubmitKind {
  def name: String = Event.EVENT_UPDATE
}
case object DELETESubmitKind extends SubmitKind {
  def name: String = Event.EVENT_DELETE
}
case object BackSubmitKind extends SubmitKind {
  def name: String = Event.EVENT_BACK
}
case object SearchSubmitKind extends SubmitKind {
  def name: String = Event.EVENT_SEARCH
}
case object ExecuteSubmitKind extends SubmitKind {
  def name: String = Event.EVENT_EXECUTE
}

case class Hiddens(
  hiddens: Record
) {
  def toKeyValues: Vector[(String, String)] = hiddens.toStringVector
}
object Hiddens {
  val empty = Hiddens(Record.empty)

  def apply(k: String, v: String): Hiddens = Hiddens(
    Record.dataApp(k -> v)
  )

  def apply(x: (String,  String), xs: (String, String)*): Hiddens = Hiddens(
    Record.createApp(x +: xs)
  )

  def scenario(v: String): Hiddens = Hiddens(ScenarioCommand.PROP_SCENARIO, v)
}

case class Hidden( // TODO refactor
//  event: Option[String],
  scenario: Option[String]
) {
  def render: NodeSeq = <div>{
    scenario.map(x => <input type="hidden" name={ScenarioCommand.PROP_SCENARIO} value={x}></input>).toList
  }</div>
}
object Hidden {
  val empty = Hidden(None)
}

sealed trait Method {
  def name: String
}
case object Get extends Method {
  def name = "GET"
}
case object Post extends Method {
  def name = "POST"
}
case object Put extends Method {
  def name = "PUT"
}
case object Delete extends Method {
  def name = "DELETE"
}

case class RequestParameter(
  query: Option[Record],
  form: Option[Record]
) extends Particle {
}
object RequestParameter {
  def parse(p: String): RequestParameter = {
    val r = Particle.RequestParameterFormat.reads(Json.parse(p))
    r match {
      case JsSuccess(s, _) => s
      case m: JsError => RAISE.syntaxErrorFault(m.toString)
    }
  }
}

case class FormColumn(
  name: String,
  datatype: Option[String],
  multiplicity: Option[String], // required
  label: Option[String],
  placeholder: Option[String]
) extends Particle
object FormColumn {
  def create(
    name: String,
    datatype: String,
    multiplicity: String,
    label: String
  ): FormColumn =
    FormColumn(name, Some(datatype), Some(multiplicity), Some(label), None)

  def parseList(p: String): List[FormColumn] = Particle.parseParticleList(p) match {
    case JsSuccess(xs, _) => xs.collect {
      case m: FormColumn => m
    }
    case m: JsError => RAISE.syntaxErrorFault(m.toString)
  }
}

case class Tabs(panes: List[TabPane]) extends Particle

case class TabPane(
  label: I18NElement,
  content: XmlContent
) extends Particle {
}
object TabPane {
  def apply(label: String, m: NodeSeq): TabPane = TabPane(I18NElement(label), XmlContent(m))

  def apply(p: Elem): TabPane = {
    val name = XmlUtils.getAttribute(p, "name", "***No name({m.hashCode})***")
    val content = XmlUtils.nodesToNode(p.child)
    TabPane(name, content)
  }
}

sealed trait Candidates extends Particle { // ? Particle
}
object Candidates {
  import org.goldenport.json.JsonUtils.Implicits._
  import Schema.json._

  implicit object PowertypeFormat extends Format[Powertype] {
    def reads(json: JsValue): JsResult[Powertype] = {
      val r = PowertypeInstance(
        JsonUtils.getInt("value", json),
        JsonUtils.toString("name", json),
        JsonUtils.getString("label", json),
        (json \ "i18nLabel") match {
          case m: JsObject => I18NStringFormat.reads(m) match {
            case JsSuccess(s, _) => Some(s)
            case JsError(e) => None
          }
          case _ => None
        }
      )
      JsSuccess(r)
    }
    def writes(o: Powertype): JsValue = {
      val a = SeqUtils.buildTupleVector[JsValue](
        Vector(
          "value" -> JsNumber(o.value),
          "name" -> JsString(o.name),
          "label" -> JsString(o.label)
        ),
        Vector(
          "i18nLabel" -> o.i18nLabel.map(I18NStringFormat.writes)
        )
      )
      JsObject(a)
    }
  }

  implicit object PowertypeClassFormat extends Format[PowertypeClass] {
    def reads(json: JsValue): JsResult[PowertypeClass] = {
      (json \ "elements") match {
        case JsDefined(js) => js match {
          case JsArray(xs) =>
            _sequence(xs.map(PowertypeFormat.reads)).
              map(PowertypeClassInstance)
          case _ => JsError(s"$json")
        }
        case _: JsUndefined => JsError(s"$json")
      }
    }
    def writes(o: PowertypeClass): JsValue = {
      val elements = JsArray(o.elements.map(PowertypeFormat.writes))
      val a = List(
        "elements" -> elements
      )
      JsObject(a)
    }
  }

  implicit object CandidatesFormat extends Format[Candidates] {
    def reads(p: JsValue): JsResult[Candidates] = p match {
      case JsString(name) => JsSuccess(NamedCandidates(name))
      case m: JsObject => PowertypeClassFormat.reads(p).map(PowertypeClassCandidates)
      case JsArray(xs) => _sequence(xs.map(PowertypeFormat.reads)).map(PowertypeCandidates)
      case _ => JsError(s"$p")
    }
    def writes(o: Candidates): JsValue = o match {
      case PowertypeClassCandidates(powertype) => PowertypeClassFormat.writes(powertype)
      case PowertypeCandidates(xs) => JsArray(xs.map(PowertypeFormat.writes))
      case NamedCandidates(name) => JsString(name)
    }
  }

  private def _sequence[T](ps: Seq[JsResult[T]]): JsResult[Seq[T]] = {
    @annotation.tailrec
    def go(ls: List[JsResult[T]], rs: Vector[T]): JsResult[Seq[T]] = ls match {
      case Nil => JsSuccess(rs)
      case x :: xs => x match {
        case JsSuccess(s, _) => go(xs, rs :+ s)
        case m: JsError => m
      }
    }
    go(ps.toList, Vector.empty)
  }
}

case class PowertypeClassCandidates(powertype: PowertypeClass) extends Candidates {
}

case class PowertypeCandidates(powertypes: Seq[Powertype]) extends Candidates {
}

case class NamedCandidates(name: String) extends Candidates {
}

case class PowertypeClassInstance(
  elements: Seq[Powertype]
) extends PowertypeClass {
  type T = Powertype
}

case class PowertypeInstance(
  value: Int,
  name: String,
  override val label: String,
  override val i18nLabel: Option[I18NString]
) extends Powertype
object PowertypeInstance {
  def apply(
    v: Option[Int],
    n: String,
    l: Option[String],
    i: Option[I18NString]
  ): PowertypeInstance =
    PowertypeInstance(v.getOrElse(-1), n, l.getOrElse(n), i)

  def apply(name: String): PowertypeInstance = PowertypeInstance(None, name, None, None)
}

case class BrokenParticle(error: JsError) extends Particle {
}

object Particle {
  import org.goldenport.json.JsonUtils.Implicits._

  implicit val DomainEntityTypeFormat = Json.format[DomainEntityType]
  implicit object DomainObjectIdFormat extends Format[DomainObjectId] {
    def reads(json: JsValue): JsResult[DomainObjectId] = json match {
      case JsString(s) => JsSuccess(StringDomainObjectId(s))
      case _ => JsError(s"Not json object: $json")
    }
    def writes(p: DomainObjectId): JsValue = RAISE.notImplementedYetDefect
  }
  implicit val DomainEntityIdFormat = Json.format[DomainEntityId]
  implicit object RecordFormat extends Format[Record] { // TODO migrate to record.
    def reads(json: JsValue): JsResult[Record] = json match {
      case m: JsObject => JsSuccess(Record.create(JsonUtils.toMapS(m)))
      case _ => JsError(s"Not json object: $json")
    }
    def writes(p: Record): JsValue = RAISE.notImplementedYetDefect
  }
  implicit val RequestParameterFormat = Json.format[RequestParameter]
  implicit object LabelIndicatorFormat extends Format[LabelIndicator] {
    def reads(json: JsValue): JsResult[LabelIndicator] = LabelIndicator.parseJsValue(json)
    def writes(p: LabelIndicator): JsValue = RAISE.notImplementedYetDefect
  }
  implicit val DomainEntityLinkFormat = Json.format[DomainEntityLink]
  implicit val BadgeFormat = Json.format[Badge]
  implicit val TitleLineleFormat = Json.format[TitleLine]
  implicit val PictureFormat = Json.format[Picture]
  implicit val CardFormat = Json.format[Card]
  implicit val XmlFormat = Json.format[Xml]
  implicit val FormColumnFormat = Json.format[FormColumn]
  // implicit object QueryFormat extends Format[Query] {
  //   def reads(json: JsValue): JsResult[Query] = json match {
  //     case m: JsObject => JsSuccess(Query(Record.create(JsonUtils.toMapS(m))))
  //     case m => JsError(s"Unknown query: $json")
  //   }
  //   def writes(p: Query): JsValue = RAISE.notImplementedYetDefect
  // }
  implicit object ParticleFormat extends Format[Particle] {
    def reads(json: JsValue): JsResult[Particle] = parseJsValue(json)
    def writes(p: Particle): JsValue = RAISE.notImplementedYetDefect
  }

  def parseParticle(p: String): JsResult[Particle] = parseJsValue(Json.parse(p))

  def parseParticleList(p: String): JsResult[List[Particle]] = parseJsValueList(Json.parse(p))

  def parseJsValueList(json: JsValue): JsResult[List[Particle]] = json match {
    case m: JsArray => JsSuccess(toParticleList(m))
    case m: JsObject => JsSuccess(List(toParticle(m)))
    case m => JsError(s"Unknown particle: $json")
  }

  def parseJsValue(json: JsValue): JsResult[Particle] = json match {
    case m: JsObject => parseJsObject(m)
    case m => JsError(s"Unknown particle: $json")
  }

  def parseJsObject(o: JsObject): JsResult[Particle] = {
    val a: Option[JsResult[Particle]] = (o \ "type").asOpt[String].map {
      case "picture" => Json.fromJson[Picture](o)
      case "card" => Json.fromJson[Card](o)
      case "xml" => Json.fromJson[Xml](o)
      case "parameter" => Json.fromJson[RequestParameter](o)
      case "column" => Json.fromJson[FormColumn](o)
      case "candidates" => Json.fromJson[Candidates](o)
      case m => JsError(s"Unknown particle: $o")
    }
    a.getOrElse {
      JsError(s"Unknown particle: $o")
    }
  }

  def toParticle(json: JsValue): Particle = json match {
    case m: JsObject => toParticle(m)
    case m => BrokenParticle(JsError(m.toString))
  }

  def toParticle(json: JsObject): Particle = Json.fromJson[Particle](json) match {
      case JsSuccess(s, _) => s
      case m: JsError => BrokenParticle(m)
  }

  def toParticleList(json: JsValue): List[Particle] = json match {
    case JsArray(xs) => xs.toList.map(toParticle)
    case m: JsObject => List(toParticle(m))
    case m => RAISE.notImplementedYetDefect
  }
}
