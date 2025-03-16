package arcadia.standalone.arcadiasite

import java.io.File
import com.typesafe.config.{Config => Hocon}
import org.goldenport.RAISE
import org.goldenport.context.Showable
import org.goldenport.io.InputSource
import org.goldenport.tree.TreeNode
import org.goldenport.tree.TreeTransformer
import org.goldenport.tree.TreeTransformer.Directive
import org.goldenport.realm.Realm
import org.goldenport.realm.RealmTransformer
import arcadia._
import arcadia.context._
import arcadia.standalone.service.ArcadiaService.PROP_STANDALONE_WEB_APPLICATION_NAME

/*
 * @since   Mar. 10, 2025
 * @version Mar. 15, 2025
 * @author  ASAMI, Tomoharu
 */
case class ArcadiaSite(
  space: Realm
) extends Showable.Control {
  import ArcadiaSite._

  protected def show_Name: String = "ArcadiaSite"
  protected def show_Print = space.print
  protected def show_String: String = space.show

  def toRealm(): Realm = space
}

object ArcadiaSite {
  case class ArcadiaTransformer(
    platformExecutionContext: PlatformExecutionContext,
    realmTransformerContext: RealmTransformer.Context,
    engine: WebEngine
  ) extends RealmTransformer {
    override protected def make_Node(node: TreeNode[Realm.Data], content: Realm.Data): Directive[Realm.Data] = {
      content match {
        case Realm.EmptyData => Directive.Empty()
        case m: Realm.StringData => _transform(node, m)
        case m: Realm.UrlData => directive_leaf(m)
        case m: Realm.FileData => directive_leaf(m)
        case m: Realm.BagData => directive_leaf(m)
        case m: Realm.ObjectData => RAISE.notImplementedYetDefect
        case m: Realm.ApplicationData => RAISE.notImplementedYetDefect
      }
    }

    private def _transform(node: TreeNode[Realm.Data], p: Realm.StringData): Directive[Realm.Data] = {
      node.getNameSuffix match {
        case Some(s) => s match {
          case "html" => _transform_page(node, p)
          case _ => Directive.AsIs()
        }
        case None => Directive.Default()
      }
    }

    private def _transform_page(node: TreeNode[Realm.Data], p: Realm.StringData): Directive[Realm.Data] = {
      val name = node.name
      val pathname = _to_relative(node.pathname)
//      val parcel: Parcel = Parcel.view(platformExecutionContext, pathname)
      val parcel: Parcel = Parcel.material(platformExecutionContext, pathname)
      val c = engine.apply(parcel)
      c match {
        case m: StringContent => directive_leaf(Realm.StringData(m.string))
        case m: XmlContent => directive_leaf(Realm.StringData(m.toHtmlString))
        case m: BinaryContent => directive_leaf(Realm.BagData(m.binary))
        case m: RedirectContent => RAISE.notImplementedYetDefect
        case m: ErrorContent => RAISE.notImplementedYetDefect
      }
    }

    private def _to_relative(path: String) = path.dropWhile(_ == '/')
  }

  def create(
    pec: PlatformExecutionContext,
    config: Hocon,
    libs: Seq[InputSource],
    realm: Realm
  ): ArcadiaSite = {
    println(s"libs: ${libs}")
    val ctx = RealmTransformer.Context.default
    val arcadia = createArcadia(pec.platformContext, config, libs, List(realm))
    val engine = createEngine(arcadia, PROP_STANDALONE_WEB_APPLICATION_NAME)
    val dependencies = engine.extendRealms
    val tf = ArcadiaTransformer(pec, ctx, engine)
    val a = realm.transform(tf)
    val xs = dependencies.map(_.transform(tf))
    val r = xs.foldLeft(a)((z, x) => z + x)
    ArcadiaSite(r)
  }

  def createEngine(arcadia: Arcadia, name: String) = arcadia.engine(name)

  def createArcadia(
    pc: PlatformContext,
    config: Hocon,
    libs: Seq[InputSource],
    standalones: Seq[Realm]
  ) = {
    val webengineconfig = WebEngine.Config.empty
    Arcadia.make(pc, webengineconfig, config, libs, standalones).take
  }
}
