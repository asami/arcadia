package arcadia.standalone.service.arcadiasite

import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner
import java.io.File
import org.goldenport.io.InputSource
import org.goldenport.collection.NonEmptyVector
import org.goldenport.realm.Realm
import org.goldenport.hocon.HoconUtils
import arcadia.context._

/*
 * @since   Mar. 11, 2025
 * @version Aug. 11, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class ArcadiaSiteSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "Arcadia site" should {
    "typical" which {
      "plain" ignore {
        Given("a plain site fixture")
        val pec: PlatformExecutionContext = PlatformExecutionContext.develop
        val config = HoconUtils.empty
        val in = Realm.create(new File("src/test/resources/site1"))

        When("the site is created")
        val r = ArcadiaSite.create(pec, config, Nil, NonEmptyVector(in))
        val output = r.print

        Then("the site representation is available")
        output should not be empty
      }
      "lib" in {
        Given("a site fixture with a base library")
        val pec: PlatformExecutionContext = PlatformExecutionContext.develop
        val config = HoconUtils.empty
        val in = Realm.create(new File("src/test/resources/site-app"))
        val libs = Vector(InputSource.file("src/test/resources/site-base"))

        When("the site is created with its library")
        val r = ArcadiaSite.create(pec, config, libs, NonEmptyVector(in))
        val output = r.print

        Then("the composed site representation is available")
        output should not be empty
      }
    }
  }
}
