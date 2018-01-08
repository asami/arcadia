package arcadia.view

import org.goldenport.Strings
import IconFactory._

/*
 * @since   Aug. 14, 2017
 * @version Dec. 30, 2017
 * @author  ASAMI, Tomoharu
 */
sealed trait IconFactory {
  def unknownIcon: String
  def getIcon(p: String): Option[String]
  def guessIcon(p: String): String = guessIconOption(p) getOrElse unknownIcon
  def guessIconOption(p: String): Option[String]
}

// See https://themify.me/themify-icons
case class ThemifyIconFactory() extends IconFactory {
  val unknownIcon = "ti-wand"
  val tiIconNames = Set(
    "wand",
    "volume",
    "user",
    "unlock",
    "unlink",
    "trash",
    "thought",
    "target",
    "tag",
    "tablet",
    "star",
    "spray",
    "signal",
    "shopping-cart",
    "shopping-cart-full",
    "settings",
    "search",
    "zoom-in",
    "zoom-out",
    "c]ut",
    "ruler",
    "ruler-pencil",
    "ruler-alt",
    "bookmark",
    "bookmark-alt",
    "reload",
    "plus",
    "pin",
    "pencil",
    "pencil-alt",
    "paint-roller",
    "paint-bucket",
    "na",
    "mobile",
    "minus",
    "medall",
    "medall-alt",
    "marker",
    "marker-alt",
    "arrow-up",
    "arrow-right",
    "arrow-left",
    "arrow-down",
    "lock",
    "location-arrow",
    "link",
    "layout",
    "layers",
    "layers-alt",
    "key",
    "import",
    "image",
    "heart",
    "heart-broken",
    "hand-stop",
    "hand-open",
    "hand-drag",
    "folder",
    "flag",
    "flag-alt",
    "flag-alt-2",
    "eye",
    "export",
    "exchange-vertical",
    "desktop",
    "cup",
    "crown",
    "comments",
    "comment",
    "comment-alt",
    "close",
    "clip",
    "angle-up",
    "angle-right",
    "angle-left",
    "angle-down",
    "check",
    "check-box",
    "camera",
    "announcement",
    "brush",
    "briefcase",
    "bolt",
    "bolt-alt",
    "blackboard",
    "bag",
    "move",
    "arrows-vertical",
    "arrows-horizontal",
    "fullscreen",
    "arrow-top-right",
    "arrow-top-left",
    "arrow-circle-up",
    "arrow-circle-right",
    "arrow-circle-left",
    "arrow-circle-down",
    "angle-double-up",
    "angle-double-right",
    "angle-double-left",
    "angle-double-down",
    "zip",
    "world",
    "wheelchair",
    "view-list",
    "view-list-alt",
    "view-grid",
    "uppercase",
    "upload",
    "underline",
    "truck",
    "timer",
    "ticket",
    "thumb-up",
    "thumb-down",
    "text",
    "stats-up",
    "stats-down",
    "split-v",
    "split-h",
    "smallcap",
    "shine",
    "shift-right",
    "shift-left",
    "shield",
    "notepad",
    "server",
    "quote-right",
    "quote-left",
    "pulse",
    "printer",
    "power-off",
    "plug",
    "pie-chart",
    "paragraph",
    "panel",
    "package",
    "music",
    "music-alt",
    "mouse",
    "mouse-alt",
    "money",
    "microphone",
    "menu",
    "menu-alt",
    "map",
    "map-alt",
    "loop",
    "location-pin",
    "list",
    "light-bulb",
    "Italic",
    "info",
    "infinite",
    "id-badge",
    "hummer",
    "home",
    "help",
    "headphone",
    "harddrives",
    "harddrive",
    "gift",
    "game",
    "filter",
    "files",
    "file",
    "eraser",
    "envelope",
    "download",
    "direction",
    "direction-alt",
    "dashboard",
    "control-stop",
    "control-shuffle",
    "control-play",
    "control-pause",
    "control-forward",
    "control-backward",
    "cloud",
    "cloud-up",
    "cloud-down",
    "clipboard",
    "car",
    "calendar",
    "book",
    "bell",
    "basketball",
    "bar-chart",
    "bar-chart-alt",
    "back-right",
    "back-left",
    "arrows-corner",
    "archive",
    "anchor",
    "align-right",
    "align-left",
    "align-justify",
    "align-center",
    "alert",
    "alarm-clock",
    "agenda",
    "write",
    "window",
    "widgetized",
    "widget",
    "widget-alt",
    "wallet",
    "video-clapper",
    "video-camera",
    "vector",
    "themify-logo",
    "themify-favicon",
    "themify-favicon-alt",
    "support",
    "stamp",
    "split-v-alt",
    "slice",
    "shortcode",
    "shift-right-alt",
    "shift-left-alt",
    "ruler-alt-2",
    "receipt",
    "pin2",
    "pin-alt",
    "pencil-alt2",
    "palette",
    "more",
    "more-alt",
    "microphone-alt",
    "magnet",
    "line-double",
    "line-dotted",
    "line-dashed",
    "layout-width-full",
    "layout-width-default",
    "layout-width-default-alt",
    "layout-tab",
    "layout-tab-window",
    "layout-tab-v",
    "layout-tab-min",
    "layout-slider",
    "layout-slider-alt",
    "layout-sidebar-right",
    "layout-sidebar-none",
    "layout-sidebar-left",
    "layout-placeholder",
    "layout-menu",
    "layout-menu-v",
    "layout-menu-separated",
    "layout-menu-full",
    "layout-media-right-alt",
    "layout-media-right",
    "layout-media-overlay",
    "layout-media-overlay-alt",
    "layout-media-overlay-alt-2",
    "layout-media-left-alt",
    "layout-media-left",
    "layout-media-center-alt",
    "layout-media-center",
    "layout-list-thumb",
    "layout-list-thumb-alt",
    "layout-list-post",
    "layout-list-large-image",
    "layout-line-solid",
    "layout-grid4",
    "layout-grid3",
    "layout-grid2",
    "layout-grid2-thumb",
    "layout-cta-right",
    "layout-cta-left",
    "layout-cta-center",
    "layout-cta-btn-right",
    "layout-cta-btn-left",
    "layout-column4",
    "layout-column3",
    "layout-column2",
    "layout-accordion-separated",
    "layout-accordion-merged",
    "layout-accordion-list",
    "ink-pen",
    "info-alt",
    "help-alt",
    "headphone-alt",
    "hand-point-up",
    "hand-point-right",
    "hand-point-left",
    "hand-point-down",
    "gallery",
    "face-smile",
    "face-sad",
    "credit-card",
    "control-skip-forward",
    "control-skip-backward",
    "control-record",
    "control-eject",
    "comments-smiley",
    "brush-alt",
    "youtube",
    "vimeo",
    "twitter",
    "time",
    "tumblr",
    "skype",
    "share",
    "share-alt",
    "rocket",
    "pinterest",
    "new-window",
    "microsoft",
    "list-ol",
    "linkedin",
    "layout-sidebar-2",
    "layout-grid4-alt",
    "layout-grid3-alt",
    "layout-grid2-alt",
    "layout-column4-alt",
    "layout-column3-alt",
    "layout-column2-alt",
    "instagram",
    "google",
    "github",
    "flickr",
    "facebook",
    "dropbox",
    "dribbble",
    "apple",
    "android",
    "save",
    "save-alt",
    "yahoo",
    "wordpress",
    "vimeo-alt",
    "twitter-alt",
    "tumblr-alt",
    "trello",
    "stack-overflow",
    "soundcloud",
    "sharethis",
    "sharethis-alt",
    "reddit",
    "pinterest-alt",
    "microsoft-alt",
    "linux",
    "jsfiddle",
    "joomla",
    "html5",
    "flickr-alt",
    "email",
    "drupal",
    "dropbox-alt",
    "css3",
    "rss",
    "rss-alte"
  )

  // ti-comment, ti-comments, ti-envelop
  private val _guess_map = Map(
    "mail" -> "ti-email",
    "message" -> "ti-comment-alt",
    "job" -> "ti-truck",
    "setup" -> "ti-settings"
  )

  private lazy val _icon_slots: Vector[Slot] = {
    val a = _guess_map.toVector.map {
      case (k, v) => Slot(k, None, v)
    }
    val b = tiIconNames.toVector.sortBy(_.length * -1).flatMap(get_slot)
    a ++ b
  }

  def getIcon(p: String): Option[String] =
    if (tiIconNames.contains(p))
      Some(s"ti-$p")
    else
      None

  def guessIconOption(p: String): Option[String] = {
    val s = p.toLowerCase
    _icon_slots.find(_.isGuessMatch(s)).map(_.icon)
  }

  protected def apply_slot(p: String, d: String): Slot = Strings.totokens(p, "-") match {
    case Nil => Slot("", None, unknownIcon)
    case x :: Nil => Slot(x, None, s"ti-$p")
    case x :: x1 :: xs =>
      if (x1 == "alt") Slot(x, None, s"ti-$p") else Slot(x, Some(x1), s"ti-$p")
  }

  protected def get_slot(p: String): Option[Slot] = Strings.totokens(p, "-") match {
    case Nil => None
    case x :: Nil => Some(Slot(x, None, s"ti-$p"))
    case x :: x1 :: Nil =>
      Some(if (x1 == "alt") Slot(x, None, s"ti-$p") else Slot(x, Some(x1), s"ti-$p"))
    case _ => None
  }
}

// http://demos.creative-tim.com/now-ui-kit/nucleo-icons.html
case class NucleoIconFactory() extends IconFactory {
  val unknownIcon = "nc-atom"
  def getIcon(p: String): Option[String] = None
  def guessIconOption(p: String): Option[String] = None
}

object IconFactory {
  val themify = ThemifyIconFactory()
  val nucleo = NucleoIconFactory()

  case class Slot(main: String, sub: Option[String], icon: String) {
    def isGuessMatch(p: String) =
      p.contains(main) && sub.fold(true)(p.contains)
  }
  // object Slot {
  //   def apply(p: String, d: String): Slot = Strings.totokens(p, "-") match {
  //     case Nil => Slot("", None, unknownTiIcon)
  //     case x :: Nil => Slot(x, None, s"ti-$p")
  //     case x :: x1 :: xs =>
  //       if (x1 == "alt") Slot(x, None, s"ti-$p") else Slot(x, Some(x1), s"ti-$p")
  //   }
  //   def get(p: String): Option[Slot] = Strings.totokens(p, "-") match {
  //     case Nil => None
  //     case x :: Nil => Some(Slot(x, None, s"ti-$p"))
  //     case x :: x1 :: Nil =>
  //       Some(if (x1 == "alt") Slot(x, None, s"ti-$p") else Slot(x, Some(x1), s"ti-$p"))
  //     case _ => None
  //   }
  // }

  def getTiIcon(p: String): Option[String] = themify.getIcon(p)
  def guessTiIcon(p: String): String = themify.guessIcon(p)
  def guessTiIconOption(p: String): Option[String] = themify.guessIconOption(p)
  def guessNcIcon(p: String): String = nucleo.guessIcon(p)
}
