package arcadia.view

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import java.net.URI
import org.goldenport.i18n.{I18NString, I18NElement}
import org.goldenport.record.v2.Column
import arcadia.context._
import arcadia.view.Renderer.TableOrder.Paging
import arcadia.view.Renderer.TableOrder.Paging.Navigation
import arcadia.view.Renderer.TableOrder.Paging.Navigation._

/*
 * @since   Oct. 29, 2023
 *  version Oct. 31, 2023
 * @version Nov.  4, 2023
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class RendererTableOrderPagingSpec extends WordSpec with Matchers with GivenWhenThen {
  val uri = new URI("http://example.com")
  val windowSize = 10

  def paging(
    offset: Int,
    pageSize: Int,
    totalSize: Int
  ) = Paging(uri, offset, pageSize, windowSize, Some(totalSize))

  private def _uri(offset: Int, limit: Int) = s"${uri}?offset=$offset&limit=$limit"

  private def _location(offset: Int, limit: Int) =
    Location(_uri(offset, limit), offset, limit)

  private def _navigation_empty() = {
    val prev = None
    val next = None
    val slots = Nil
    Navigation(prev, next, slots)
  }

  case class Builder(start: Int, limit: Int, total: Int) {
    def build(): List[Slot] =
      _build(_page_number(start), start, Vector.empty).toList

    private def _page_number(offset: Int) = offset / limit

    @annotation.tailrec
    private def _build(number: Int, index: Int, xs: Vector[Slot]): Vector[Slot] =
      if (index >= total || number >= windowSize)
        xs
      else
        _build(number + 1, index + limit, xs = xs :+ Slot(number, _location(index, limit)))
  }

  def navigation(offset: Int, limit: Int, total: Int) = {
    val slots = Builder(offset, limit, total).build()
    val prev = slots.headOption match {
      case Some(s) =>
        if (s.location.offset == 0) {
          None
        } else {
          val a = math.max(0, offset - (limit * windowSize))
          Some(Prev(_location(a, limit)))
        }
      case None => None
    }
    val next = slots.lastOption match {
      case Some(s) =>
        // println(s"a: ${s.location.offset}")
        // println(s"a: ${limit}")
        // println(s"a: ${s.location.offset + limit}")
        // println(s"b: ${total}")
        if (s.location.offset + limit < total)
          Some(Next(_location(offset + (limit * windowSize), limit)))
        else
          None
      case None => None
    }
    Navigation(prev, next, slots)
  }

  "RendererTableOrderPaging" should {
    "typical" which {
      "paging" in {
        val pg = paging(0, 20, 100)
        val nav = pg.navigation
        nav should be(navigation(0, 20, 100))
      }
      "paging2" in {
        val pg = paging(0, 20, 200)
        val nav = pg.navigation
        nav should be(navigation(0, 20, 200))
      }
      "paging3" in {
        val pg = paging(0, 20, 300)
        val nav = pg.navigation
        nav should be(navigation(0, 20, 300))
      }
      "paging4" in {
        val pg = paging(1, 1, 4)
        val nav = pg.navigation
        println(nav)
        nav should be(navigation(1, 1, 4))
      }
    }
  }
}
