package arcadia.rule

import java.util.Locale
import java.net.URI
import org.goldenport.i18n.I18NElement
import org.goldenport.record.v2._
import org.goldenport.record.v3.IRecord
import arcadia.context._
import arcadia.model._

/*
 * @since   Apr. 25, 2019
 *  version Apr. 30, 2019
 * @version May.  1, 2019
 * @author  ASAMI, Tomoharu
 */
case class ResetPasswordRule(
  passwordRule: PasswordRule,
  baseUrl: Option[String] = None,
  mailTitle: Option[String] = None,
  mailContent: Option[String] = None,
  mailFrom: Option[String] = None,
  titleImage: Option[String] = None,
  titleText: Option[String] = None,
  mainTitleImage: Option[String] = None,
  mainTitleText: Option[String] = None,
  mainText: Option[String] = None,
  completeTitleImage: Option[String] = None,
  completeTitleText: Option[String] = None,
  errorTitleImage: Option[String] = None,
  errorTitleText: Option[String] = None,
  completeRedirectUrl: Option[String] = None,
  errorRedirectUrl: Option[String] = None,
  errorMailAddress: Option[String] = None
) {
  def toParameters: Parameters = {
    val constraits = passwordRule.toConstraints
    val desc = passwordRule.labelI18N
    val placeholder = passwordRule.placeholder
    val validators = passwordRule.toValidators
    Parameters(
      List(
        Parameter.create(PasswordRule.PROP_PASSWORD, XPassword, constraits),
        Parameter.create(PasswordRule.PROP_PASSWORD_REPEAT, XPassword)
      ),
      validators
    )
  }

  def toDirectiveModel(
    uri: URI,
    okLabel: Option[I18NElement],
    arguments: IRecord,
    error: Option[Invalid] = None
  ): InvokeDirectiveFormModel = {
    val title = mainTitleText.map(I18NElement(_))
    val description = mainText.map(I18NElement(_))
    val parameters = toParameters
    InvokeDirectiveFormModel(
      uri,
      Post,
      title,
      description,
      okLabel,
      parameters,
      arguments,
      true,
      error
    )
  }
}

object ResetPasswordRule {
  val default = ResetPasswordRule(PasswordRule.default)
}
