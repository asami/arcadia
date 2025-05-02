package arcadia.service

import scala.util.matching.Regex.Match
import org.goldenport.context.{Consequence, Conclusion}
import org.goldenport.record.v3.IRecord
import org.goldenport.util.RegexUtils
import arcadia._
import arcadia.context._

/*
 * @since   Mar. 20, 2022
 *  version Mar. 20, 2022
 *  version Sep. 26, 2022
 * @version Oct. 22, 2022
 * @author  ASAMI, Tomoharu
 */
class SystemService(pc: PlatformContext) extends Service {
  def name = "system"

  def invoke(op: InvokeOperationCommand): Response = {
    op.request.operationName match {
      case "loopback" => _loopback(op)
      case _ => Response.notFound()
    }
  }

  private def _loopback(op: InvokeOperationCommand) = {
    val query = op.request.query
    val form = op.request.form
    _find_error(form) getOrElse Response.record(query + form)
  }

  private def _find_error(p: IRecord): Option[Response] = {
    val s = p.fields.map(_.asString)
    s.flatMap(SystemService.loopback.findError).headOption.map { x =>
      Response.error(x.code, x.message)
    }
  }
}

object SystemService {
  object loopback {
    val errorRegex = """[#]([\d]*)[ ]?(.*)""".r

    def findError(p: String): Option[Conclusion] =
      errorRegex.findFirstMatchIn(p).flatMap { x =>
        val r = for {
          code <- RegexUtils.cAsInt(x, 1)
          msg <- RegexUtils.cGetString(x, 2)
        } yield Conclusion.error(code, msg)
        r.toOption
      }
  }
}
