package arcadia.controller

import org.goldenport.RAISE
import arcadia._
import arcadia.scenario.Intent

/*
 * @since   May. 28, 2020
 * @version May. 28, 2020
 * @author  ASAMI, Tomoharu
 */
case class Move(
  forward: Move.Route,
  backward: Option[Move.Route],
  error: Option[Move.Route]
) {
  def success(p: Intent): Intent = forward(p)
  def error(p: Intent): Intent = error.map(_.apply(p)).getOrElse(p)
}

object Move {
  sealed trait Route {
    def apply(p: Intent): Intent
  }
  object Route {
    def create(redirect: Option[String], view: Option[String]): Route =
      redirect.map(RedirectRoute).
        orElse(view.map(ViewRoute)).
        getOrElse(RAISE.invalidArgumentFault("No redirect and view."))
  }

  case object TopRoute extends Route {
    def apply(p: Intent): Intent = p.setRedirect("index")
  }

  case class ViewRoute(uri: String) extends Route {
    def apply(p: Intent): Intent = p.withViewCommand(uri)
  }

  case class RedirectRoute(uri: String) extends Route {
    def apply(p: Intent): Intent = p.setRedirect(uri)
  }

  def success2error2(
    successredirect: Option[String],
    successview: Option[String],
    errorredirect: Option[String],
    errorview: Option[String]
  ): Move = Move(
    Route.create(successredirect, successview),
    None,
    Some(Route.create(errorredirect, errorview))
  )
}
