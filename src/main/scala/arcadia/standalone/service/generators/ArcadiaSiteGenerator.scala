package arcadia.standalone.service.generators

import com.typesafe.config.{Config => Hocon}
import org.goldenport.io.InputSource
import org.goldenport.realm.Realm
import org.goldenport.realm.Realm.{Data, ObjectData, StringData}
import org.goldenport.realm.RealmTransformer
import org.goldenport.tree.TreeNode
import org.goldenport.util.StringUtils
import arcadia.standalone.arcadiasite.ArcadiaSite

/*
 * @since   Mar. 10, 2025
 * @version Mar. 13, 2025
 * @author  ASAMI, Tomoharu
 */
class ArcadiaSiteGenerator(
  val context: Context
) extends GeneratorBase {
  import ArcadiaSiteGenerator._

  def generate(
    libs: Seq[InputSource],
    realm: Realm
  ): Realm = {
    val site = ArcadiaSite.create(context.platformExecutionContext, context.properties, libs, realm)
    val out = site.toRealm()
    val r = Realm.create()
    r.merge("arcadiasite.d", out)
  }
}

object ArcadiaSiteGenerator {
}
