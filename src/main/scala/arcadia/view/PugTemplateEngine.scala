package arcadia.view

import scala.xml._
import scala.util.control.NonFatal
import scala.collection.JavaConverters._
import java.io.StringReader
import org.fusesource.scalate.TemplateSource
import de.neuland.pug4j._
import org.goldenport.io.InputSource
import org.goldenport.io.FileInputSource
import org.goldenport.io.StringInputSource
import org.goldenport.xml.XmlUtils
import arcadia._
import arcadia.context._

/*
 * @since   Mar. 19, 2025
 * @version Mar. 21, 2025
 * @author  ASAMI, Tomoharu
 */
class PugTemplateEngine(val platform: PlatformContext) extends TemplateEngine {
  def isAccept(p: TemplateSource) = isAccept(to_inputsource(p))

  def isAccept(p: InputSource) = p.getSuffix.fold(false) {
    case "pug" => true
    case "jade" => true
    case _ => false
  }

  def shutdown() = {}

  def layoutAsNodes(template: TemplateSource, bindings: ViewEngine.Bindings): NodeSeq =
    layoutAsNodes(to_inputsource(template), bindings)

  def layoutAsNodes(p: InputSource, bindings: ViewEngine.Bindings): NodeSeq = try {
    val model = bindings.bindings.asJava
    val s0 = p match {
      case m: FileInputSource =>
        val f = Pug4J.getTemplate(m.file.getAbsolutePath)
        Pug4J.render(f, model)
    }
    val s = ViewEngine.evalExpression(s0, bindings)
    XmlUtils.parseNodeSeq(s)
  } catch {
    case NonFatal(e) => Text(s"Pug error: ${e.toString}")
  }
}

object PugTemplateEngine {
}
