package arcadia.standalone.arcadiasite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import java.io.File
import java.net.URI
import org.goldenport.io.InputSource
import org.goldenport.realm.Realm
import org.goldenport.hocon.HoconUtils
import arcadia.context._

/*
 * @since   Mar. 11, 2025
 * @version Mar. 15, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class ArcadiaSiteSpec extends WordSpec with Matchers with GivenWhenThen {
  "ArcadiaSiteSpec" should {
    val pec: PlatformExecutionContext = PlatformExecutionContext.develop
    val config = HoconUtils.empty
    "typical" which {
      "plain" ignore {
        val in = Realm.create(new File("src/test/resources/site1"))
        val r = ArcadiaSite.create(pec, config, Nil, in)
        println(r.print)
      }
      "lib" in {
        val in = Realm.create(new File("src/test/resources/site-app"))
        val libs = Vector(InputSource.file("src/test/resources/site-base"))
        val r = ArcadiaSite.create(pec, config, libs, in)
        println("lib:" + r.print)
      }
    }
  }
}
