package arcadia.view

import org.goldenport.Strings
import IconFactory._

/*
 * @since   Aug. 14, 2017
 *  version Dec. 30, 2017
 * @version Mar. 13, 2018
 * @author  ASAMI, Tomoharu
 */
sealed trait IconFactory {
  def unknownIcon: String
  def getIcon(p: String): Option[String]
  def guessIcon(p: String): String = guessIconOption(p) getOrElse unknownIcon
  def guessIconOption(p: String): Option[String]
}

trait IconFactoryBase extends IconFactory {
  def icons: PartialFunction[String, String]
  def guessIcons: PartialFunction[String, String]
  def getIcon(p: String) = icons.lift(p)
  def guessIconOption(p: String) = getIcon(guessIcons.lift(p).getOrElse(p))
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

// https://demos.creative-tim.com/now-ui-dashboard-pro/examples/components/icons.html
case class NowUiIconFactory() extends IconFactoryBase {
  private val _icons = Vector(
    "arrows-1_cloud-download-93",
    "arrows-1_cloud-upload-94",
    "arrows-1_minimal-down",
    "arrows-1_minimal-left",
    "arrows-1_minimal-right",
    "arrows-1_minimal-up",
    "arrows-1_refresh-69",
    "arrows-1_share-66",
    "business_badge",
    "business_bank",
    "business_briefcase-24",
    "business_bulb-63",
    "business_chart-bar-32",
    "business_chart-pie-36",
    "business_globe",
    "business_money-coins",
    "clothes_tie-bow",
    "design_app",
    "design_bullet-list-67",
    "design_image",
    "design_palette",
    "design_scissors",
    "design_vector",
    "design-2_html5",
    "design-2_ruler-pencil",
    "emoticons_satisfied",
    "files_box",
    "files_paper",
    "files_single-copy-04",
    "health_ambulance",
    "loader_gear",
    "loader_refresh",
    "location_bookmark",
    "location_compass-05",
    "location_map-big",
    "location_pin",
    "location_world",
    "media-1_album",
    "media-1_button-pause",
    "media-1_button-play",
    "media-1_button-power",
    "media-1_camera-compact",
    "media-2_note-03",
    "media-2_sound-wave",
    "objects_diamond",
    "objects_globe",
    "objects_key-25",
    "objects_planet",
    "objects_spaceship",
    "objects_support-17",
    "objects_umbrella-13",
    "education_agenda-bookmark",
    "education_atom",
    "education_glasses",
    "education_hat",
    "education_paper",
    "shopping_bag-16",
    "shopping_basket",
    "shopping_box",
    "shopping_credit-card",
    "shopping_delivery-fast",
    "shopping_shop",
    "shopping_tag-content",
    "sport_trophy",
    "sport_user-run",
    "tech_controller-modern",
    "tech_headphones",
    "tech_laptop",
    "tech_mobile",
    "tech_tablet",
    "tech_tv",
    "tech_watch-time",
    "text_align-center",
    "text_align-left",
    "text_bold",
    "text_caps-small",
    "gestures_tap-01",
    "transportation_air-baloon",
    "transportation_bus-front-12",
    "travel_info",
    "travel_istanbul",
    "ui-1_bell-53",
    "ui-1_calendar-60",
    "ui-1_check",
    "ui-1_lock-circle-open",
    "ui-1_send",
    "ui-1_settings-gear-63",
    "ui-1_simple-add",
    "ui-1_simple-delete",
    "ui-1_simple-remove",
    "ui-1_zoom-bold",
    "ui-2_chat-round",
    "ui-2_favourite-28",
    "ui-2_like",
    "ui-2_settings-90",
    "ui-2_time-alarm",
    "ui-1_email-85"
  )
  val unknownIcon = "objects_planet"
  val icons = {
    def intp(p: String) = try {
      p.toInt
      true
    } catch {
      case m: NumberFormatException => false
    }
    val a: Map[String, String] = _icons.map { s =>
      val x = Strings.totokens(s, "_").tail.mkString("_")
      val xs = Strings.totokens(x, "-")
      val xs1 = if (intp(xs.last)) xs.init else xs
      xs1.mkString("-") -> s
    }.toMap
    val b = Map(
      "cart" -> "shopping_cart-simple",
      "delivery" -> "shopping_delivery-fast",
      "user_circle" -> "users_circle-08",
      "user" -> "users_single-02"
    )
    a ++ b
  }
  val guessIcons = Map(
    "mail" -> "email",
    "message" -> "bag",
    "job" -> "delivery",
    "setup" -> "settings-gear",
    "dashboard" -> "chart-bar",
    "media" -> "palette",
    "commerce" -> "cart",
    "cache" -> "basket",
    "system" -> "atom",
    "resource" -> "briefcase"
  )
}

object IconFactory {
  lazy val themify = ThemifyIconFactory()
  lazy val nucleo = NucleoIconFactory()
  lazy val nowui = NowUiIconFactory()

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
  def guessNowUiIcon(p: String): String = nowui.guessIcon(p)
}
