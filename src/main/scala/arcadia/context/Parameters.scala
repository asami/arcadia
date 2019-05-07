package arcadia.context

import scalaz._, Scalaz._
import org.goldenport.record.v3.IRecord
import org.goldenport.record.v2._
import org.goldenport.record.v2.Validator._
import arcadia._
import arcadia.view.RenderStrategy

/*
 * @since   Apr. 25, 2019
 * @version May.  8, 2019
 * @author  ASAMI, Tomoharu
 */
case class Parameters(
  parameters: List[Parameter],
  validators: List[Validator]
) {
  def length = parameters.length

  def validate(p: IRecord)(implicit ctx: RenderStrategy): ValidationResult = {
    val columns = parameters.map(_.toColumn)
    val schema = Schema(columns, validators = validators) // TODO validator
    schema.validate(p)
  }
}

object Parameters {
  def create(p: Parameter, ps: Parameter*): Parameters =
    Parameters(p +: ps.toList, Nil)

  def resolve(parcel: Parcel, parameters: Parameters) =
    parameters.copy(parameters = Parameter.resolve(parcel, parameters.parameters))

  def toSchema(p: Parameters)(implicit ctx: RenderStrategy): Schema =
    Schema(p.parameters.map(_.toColumn), validators = p.validators)
}
