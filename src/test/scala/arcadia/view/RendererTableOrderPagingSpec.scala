package arcadia.view

import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner
import java.net.URI
import arcadia.context._
import arcadia.view.Renderer.TableOrder.Paging
import arcadia.view.Renderer.TableOrder.Paging.Navigation
import arcadia.view.Renderer.TableOrder.Paging.Navigation._

/*
 * @since   Oct. 29, 2023
 *  version Oct. 31, 2023
 * @version Aug. 11, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class RendererTableOrderPagingSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  private val _uri = new URI("http://example.com")
  private val _window_size = 10

  private def _paging(
    offset: Int,
    pagesize: Int,
    totalsize: Int
  ) = Paging(_uri, offset, pagesize, _window_size, Some(totalsize))

  private def _uri_string(offset: Int, limit: Int) = s"${_uri}?offset=$offset&limit=$limit"

  private def _location(offset: Int, limit: Int) =
    Location(_uri_string(offset, limit), offset, limit)

  "RendererTableOrderPaging" should {
    "typical" which {
      "calculate the first result window" in {
        Given("a first page with five twenty-item slots")
        val pg = _paging(0, 20, 100)

        When("the paging navigation is calculated")
        val nav = pg.navigation

        Then("the navigation matches the first-page window")
        nav should be(Navigation(
          None,
          None,
          List(
            Slot(0, _location(0, 20)),
            Slot(1, _location(20, 20)),
            Slot(2, _location(40, 20)),
            Slot(3, _location(60, 20)),
            Slot(4, _location(80, 20))
          )
        ))
      }
      "omit the next-window link at the exact-window boundary" in {
        Given("a first page with exactly ten twenty-item slots")
        val pg = _paging(0, 20, 200)

        When("the paging navigation is calculated")
        val nav = pg.navigation

        Then("the navigation has no next-window link")
        nav should be(Navigation(
          None,
          None,
          List(
            Slot(0, _location(0, 20)),
            Slot(1, _location(20, 20)),
            Slot(2, _location(40, 20)),
            Slot(3, _location(60, 20)),
            Slot(4, _location(80, 20)),
            Slot(5, _location(100, 20)),
            Slot(6, _location(120, 20)),
            Slot(7, _location(140, 20)),
            Slot(8, _location(160, 20)),
            Slot(9, _location(180, 20))
          )
        ))
      }
      "retain the first window for several result windows" in {
        Given("a first page with results spanning several windows")
        val pg = _paging(0, 20, 300)

        When("the paging navigation is calculated")
        val nav = pg.navigation

        Then("the navigation matches the first window")
        nav should be(Navigation(
          None,
          Some(Next(_location(200, 20))),
          List(
            Slot(0, _location(0, 20)),
            Slot(1, _location(20, 20)),
            Slot(2, _location(40, 20)),
            Slot(3, _location(60, 20)),
            Slot(4, _location(80, 20)),
            Slot(5, _location(100, 20)),
            Slot(6, _location(120, 20)),
            Slot(7, _location(140, 20)),
            Slot(8, _location(160, 20)),
            Slot(9, _location(180, 20))
          )
        ))
      }
      "calculate navigation from a nonzero one-item offset" in {
        Given("a one-item page after the initial offset")
        val pg = _paging(1, 1, 4)

        When("the paging navigation is calculated")
        val nav = pg.navigation

        Then("the navigation matches the offset page")
        nav should be(Navigation(
          Some(Prev(_location(0, 1))),
          None,
          List(
            Slot(1, _location(1, 1)),
            Slot(2, _location(2, 1)),
            Slot(3, _location(3, 1))
          )
        ))
      }
    }
  }
}
