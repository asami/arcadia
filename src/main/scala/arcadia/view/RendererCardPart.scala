package arcadia.view

import scala.xml.{NodeSeq, Group, Elem, Node, Text}
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.record.v2.{Record => _, Table => _, _}
import org.goldenport.i18n.{I18NString, I18NElement}
import org.goldenport.xml.XmlUtils
import org.goldenport.util.{DateTimeUtils, DateUtils, StringUtils, SeqUtils}
import arcadia.model._
import Renderer._

/*
 * @since   Apr. 15, 2018
 *  version Apr. 30, 2018
 *  version May.  1, 2018
 *  version Jul.  8, 2018
 *  version Sep.  1, 2018
None *  version Apr. 16, 2019
None * @version Nov. 28, 2023
 * @author  ASAMI, Tomoharu
 */
trait RendererCardPart { self: Renderer =>
  protected def to_card(rec: IRecord): Card = {
    val icon = picture_icon(rec)
    val title = get_title(rec)
    val subtitle = get_subtitle(rec)
    val header = TitleLine(title, subtitle).toOption
    val content = get_content(rec)
    val summary = get_content_summary(rec)
    val link = get_link(rec)
    Card.create(icon, header, content, summary, link, rec)
  }

  protected def to_card(table: Table, rec: IRecord): Card =
    strategy.theme match {
      case MyColorTheme => _to_card_productclass_mycolor(table, rec)
      case _ => _to_card(table, rec)
    }

  private def _to_card_productclass_mycolor(table: Table, rec: IRecord): Card = {
    val icon = picture_icon(rec)
    val title = get_title(rec).map(_(locale)) getOrElse Text("")
    val price = Text("10,000") // TODO
    val content =
      <div class="c-card__textArea">
        <div class="c-card__textArea__name">{title}</div>
        <div class="c-card__textArea__price">{title}</div>
      </div>
    val link = get_link(table, rec)
    Card.create(icon, None, Some(content), None, link, rec)
  }

  private def _to_card(table: Table, rec: IRecord): Card = {
    val icon = picture_icon(rec)
    val title = get_title(rec)
    val subtitle = get_subtitle(rec)
    val header = TitleLine(title, subtitle).toOption
    val content = get_content(rec)
    val summary = get_content_summary(rec)
    val link = get_link(table, rec)
    Card.create(icon, header, content, summary, link, rec)
  }

  protected def card(rec: IRecord): NodeSeq = card(to_card(rec))

  // TableCardModel 
  protected def card(
    imagetop: Option[Picture],
    header: Option[TitleLine],
    footer: Option[TitleLine],
    content: NodeSeq
  ): NodeSeq = card(Card.create(imagetop, header, footer, content, None))

  protected def card(card: Card): NodeSeq =
    card_component(card) getOrElse card_full(card)

  protected def card_component(card: Card): Option[NodeSeq] =
    _card_component(strategy.cardKind, card)

  private def _card_component(kind: CardKind, card: Card): Option[NodeSeq] =
    kind match {
      case m: ComponentCard => strategy.viewContext.flatMap { x =>
        val a = x.parcel.withModel(CardModel(card, s"card__${kind.name}"))
        strategy.viewContext.flatMap(_.engine.renderComponentOption(a))
      }
      case ImageTitleCard => None
      case _ => None
    }

  protected def card_in_grid(p: Card): NodeSeq = {
    val card = {
      val imagetop: Option[Picture] =
        if (strategy.cardKindInGrid.isImageTop)
          p.image_top // TODO default
        else
          None
      val header: Option[TitleLine] =
        if (strategy.cardKindInGrid.isHeader)
          p.header orElse Some(TitleLine.blank)
        else
          None
      val footer: Option[TitleLine] =
        if (strategy.cardKindInGrid.isFooter)
          p.footer orElse Some(TitleLine.blank)
        else
          None
      val content: Option[I18NElement] =
        if (strategy.cardKindInGrid.isContent)
          p.content orElse Some(I18NElement(""))
        else
          None
      val summary: Option[I18NElement] =
        if (strategy.cardKindInGrid.isContent)
          p.summary
        else
          None
      Card(imagetop, header, footer, content, summary, p.link, p.record)
    }
    card_component_in_grid(card) getOrElse card_full(card)
  }

  protected def card_component_in_grid(card: Card): Option[NodeSeq] =
    _card_component(strategy.cardKindInGrid, card)

  protected def card_full(card: Card): NodeSeq = {
    strategy.theme match {
      case m: Bootstrap5RenderThemeBase => card_bootstrap(card)
      case m: Bootstrap4RenderThemeBase => card_bootstrap(card)
      case m: Bootstrap3RenderThemeBase => card_paperdashboard(card)
      case m if m.isCardDiv => card_div(card)
      case m if m.isCardTable => card_table(card)
      case m => card_table(card)
    }
  }

  // Bootstrap 4
  protected def card_bootstrap(card: Card): Elem = {
    // def img(p: Picture): Elem = XmlUtils.element("img",
    //   SeqUtils.buildTupleVector(
    //     Vector(
    //       "class" -> "card-img-top",
    //       "src" -> p.src.toString
    //     ),
    //     Vector(
    //       "alt" -> p.alt.map(node).map(_.text)
    //     )
    //   ),
    //   Nil
    // )
    def img(p: Picture) = card_img(p, "card-img-top")
    def body: NodeSeq = Group(
      card.image_top.map(i =>
        i.href.fold {
          img(i)
        } { href =>
          <a href={href.toString} target="_blank">{img(i)}</a>
        }
      ).toVector ++ card.header.map(h =>
        <div class="card-header">{
          card_title_bootstrap(h)
        }</div>
      ).toVector ++ card.content.map(c => <div class="card-body">{
        <div class="card-text">{c(locale)}</div>
      }</div>).toVector ++ card.footer.map(f =>
        <div class="card-footer">{
          card_title_bootstrap(f)
        }</div>
      ).toVector
    )
    def bodywithlink: NodeSeq =
      card.link.fold(body) { link =>
        link.getDataHref(render_context).fold(body) { href =>
          <a class={theme_card.css.table.anchor} href={href.toString}>{body}</a>
        }
      }
    <div class="card"> {
      bodywithlink
    } </div>
  }

  def card_title_bootstrap(p: TitleLine): List[Node] = {
    List(
      p.title.map(t => <h4 class="card-title">{t(locale)}</h4>),
      p.subtitle.map(d => <p class="card-subtitle">{d(locale)}</p>)
    ).flatten
  }

  protected def card_paperdashboard(card: Card): Elem = {
    <div class="card">{
      card.image_top.map(picture(_)) ++ card.header.map(h =>
        <div class="header">{
          List(
            h.title.map(t => <h4 class="title">{t(locale)}</h4>),
            h.subtitle.map(d => <p class="category">{d(locale)}</p>)
          ).flatten
        }</div>
      ) ++ card.content.map(c =>
        <div class="content">{
          c(locale) ++ List(
            card.footer.map(f =>
              <div class="footer">{
                List(
                  f.title.map(t => <h4 class="title">{t(locale)}</h4>),
                  f.subtitle.map(d => <p class="category">{d(locale)}</p>)
                ).flatten
              }</div>
            )
          ).flatten
        }</div>
      )
    }</div>
  }

  protected def card_table(card: Card): Elem = {
    def render = {
      def tl(p: TitleLine): Vector[Node] = Vector(
        p.title.map(x => <h3>{x(locale)}</h3>),
        p.subtitle.map(x => <p>{x(locale)}</p>)
      ).flatten
      <table> {
        Vector(
          card.image_top.toVector.flatMap(picture).map(x => <tr><td>{x}</td></tr>), // XXX css
          card.header.toVector.flatMap(tl).map(x => <tr><td>{x}</td></tr>), // XXX css
          card.summary.toVector.flatMap(_(locale)).map(x => <tr><td>{x}</td></tr>), // XXX css
          card.footer.toVector.flatMap(tl).map(x => <tr><td>{x}</td></tr>)
        ).flatten // XXX css
      }</table>
    }
    def data: NodeSeq = card.image_top.fold(render)(it =>
      if (card.isImageTopOnly)
        picture(it)
      else
        render
    )
    def datawithlink: NodeSeq = card.link.fold(data) { link =>
      link.getDataHref(render_context).fold(data) { href =>
        <a class={theme_card.css.table.anchor} href={href.toString}>{data}</a>
      }
    }
    <td class={theme_card.css.table.td}>{datawithlink}</td>
  }

  protected def card_div(card: Card): Elem = {
    def f(p: Picture): Elem = { // MyColor
      <div class="c-card__imageArea">
        <img class="c-card__imageArea__image" src={p.src.toString} alt={p.alt.map(node)} />
      </div>
    }
    def render = {
      def tl(p: TitleLine): Vector[Node] = Vector(
        p.title.map(x => <h3>{x(locale)}</h3>),
        p.subtitle.map(x => <p>{x(locale)}</p>)
      ).flatten
      <div class={theme_card.css.div.card}> {
        Vector(
          card.image_top.toVector.flatMap(f).map(x => <div class={theme_card.css.div.imageTop}>{x}</div>),
          card.header.toVector.flatMap(tl).map(x => <div class={theme_card.css.div.header}>{x}</div>),
          card.content.toVector.flatMap(_(locale)).map(x => <div class={theme_card.css.div.content}>{x}</div>),
          card.footer.toVector.flatMap(tl).map(x => <div class={theme_card.css.div.footer}>{x}</div>)
        ).flatten
      }</div>
    }
    def data: NodeSeq = card.image_top.fold(render)(it =>
      if (card.isImageTopOnly)
        picture(it)
      else
        render
    )
    def datawithlink: NodeSeq = card.link.fold(data) { link =>
      link.getDataHref(render_context).fold(data) { href =>
        <a class={theme_card.css.div.anchor} href={href.toString}>{data}</a>
      }
    }
    <div class={theme_card.css.div.container}>{datawithlink}</div>
  }

  protected def card_in_list(p: Card): Elem = {
    card_component_in_list(p) getOrElse card_media_object(p)
  }

  protected def card_in_list_body(p: Card): NodeSeq =
    Group(card_in_list(p).child)

  protected def card_component_in_list(p: Card): Option[Elem] =
    _card_component(strategy.cardKind, p) map {
      case m: Elem => m
      case m => <div>{m}</div>
    }

  protected def card_media_object(p: Card): Elem =
    strategy.theme match {
      case m: Bootstrap4RenderThemeBase => card_media_object_bootstrap(p)
      case m => card_media_object_div(p)
    }

  protected def card_media_object_bootstrap(p: Card) =
    <div class="media">{card_media_object_body_bootstrap(p)}</div>

  // TODO card layout mechanism for list
  protected def card_media_object_body_bootstrap(p: Card): Group = {
    def img(p: Picture): Elem = card_img(p, "mr-3 arcadia-list-icon") // TODO
    val i = p.image_top.map(img)
    val b = <div class="media-body">{
      xmlutils.childrenOption(
        p.header.flatMap(_.title.map(x => <h5 class="mt-0 text-trunate">{x(locale)}</h5>)),
        p.summary.map(x => <div class="text-trunate">{x(locale)}</div>)
      )
    }</div>
    Group(i.toVector ++ Vector(b))
  }

  // migrate to XmlUtils
  object xmlutils {
    def element(
      elementname: String,
      attrs: Seq[(String, String)],
      oattrs: Seq[(String, Option[String])] = Nil
    )(children: Seq[Option[NodeSeq]]): Elem =
      XmlUtils.element(
        elementname,
        SeqUtils.buildTupleVector(attrs, oattrs),
        Nil
      )

    def childrenOption(p: Option[NodeSeq], ps: Option[NodeSeq]*): Seq[NodeSeq] =
      childrenOption(p +: ps)

    def childrenOption(ps: Seq[Option[NodeSeq]]): Seq[NodeSeq] = ps.flatten
  }

  protected def card_img(p: Picture, classname: String): Elem =
    XmlUtils.element("img",
      SeqUtils.buildTupleVector(
        Vector(
          "class" -> classname,
          "src" -> p.src.toString
        ),
        Vector(
          "alt" -> p.alt.map(node).map(_.text)
        )
      ),
      Nil
    )

  protected def card_media_object_div(p: Card) = card_media_object_bootstrap(p) // XXX
}
