package arcadia.rule

import java.util.Locale
import org.goldenport.util.StringUtils
import org.goldenport.i18n.{I18NString, I18NMessage}
import org.goldenport.record.v2._

/*
 * @since   Apr. 25, 2019
 *  version Apr. 30, 2019
 *  version May.  1, 2019
 * @version Apr. 17, 2020
 * @author  ASAMI, Tomoharu
 */
case class PasswordRule(
  minLength: Option[Int],
  maxLength: Option[Int],
  characterRule: PasswordRule.CharacterRule
) extends Constraint {
  def validate(password: String): Boolean = {
    val s = password.length
    minLength.map(_ <= s).getOrElse(true) &&
    maxLength.map(x => s <= x).getOrElse(true) &&
    characterRule.validate(password)
  }

  def labelI18N: I18NString = I18NString(label(Locale.US)) // TODO

  def label(locale: Locale) = {
    val min = minLength.map(x => s"${x}文字以上") getOrElse ""
    val max = maxLength.map(x => s"${x}文字以下") getOrElse ""
    val delimiter = if (min.nonEmpty && max.nonEmpty) "、" else ""
    val of = if (min.nonEmpty || max.nonEmpty) "の" else ""
    s"""${min}${delimiter}${max}${of}${characterRule.label(locale)}"""
  }

  // def label(locale: String) = {
  //   val min = minLength.map(x => s"${x}文字以上") getOrElse ""
  //   val max = maxLength.map(x => s"${x}文字以下") getOrElse ""
  //   val delimiter = if (min.nonEmpty && max.nonEmpty) "、" else ""
  //   val of = if (min.nonEmpty || max.nonEmpty) "の" else ""
  //   s"""${min}${delimiter}${max}${of}${characterRule.label(locale)}"""
  // }

  // def placeholder(locale: String) = s"""パスワード ※${label(locale)}(必須)"""

  def placeholder: I18NMessage = I18NMessage(s"""パスワード ※{0}(必須)""", Vector(labelI18N))

  def toConstraints: List[Constraint] = List(this)

  def validate(datatype: DataType, value: String, record: Record): Option[ValidationResult] =
    datatype match {
      case XPassword =>
        if (validate(value)) {
          Some(Valid)
        } else {
          val cause = label(Locale.US) // TODO
          val msg = s"""パスワードが${cause}ではありません。"""
          Some(ValueDomainFailure(msg, value))
        }
      case _ => None
    }

  def toValidators: List[Validator] = List(PasswordRule.PasswordRepeatValidator)
}

object PasswordRule {
  val PROP_PASSWORD = "password"
  val PROP_PASSWORD_REPEAT = "password_repeat"

  val default = PasswordRule(Some(4), Some(32), AlphabetNumberRule)
  val all = PasswordRule(None, None, AllCharactersRule)
  val alphabetNumberCharacter = PasswordRule(Some(4), Some(32), AlphabetNumberCharacterRule)

  trait CharacterRule {
    def label(locale: Locale): String
    def validate(password: String): Boolean
  }

  case object AlphabetNumberRule extends CharacterRule {
    def label(locale: Locale) = "英数字"
    def validate(password: String) = password.forall(c =>
      ('a' <= c && c <= 'z') ||
        ('A' <= c && c <= 'Z') ||
        ('0' <= c && c <= '9'))
  }

  case object AlphabetNumberMixRule extends CharacterRule {
    def label(locale: Locale) = "英数字"
    def validate(password: String) = {
      StringUtils.isAsciiAlphabetNumberString(password) &&
      password.headOption.fold(false) { x =>
        if (StringUtils.isAsciiAlphabetChar(x))
          !StringUtils.isAsciiAlphabetString(password.tail)
        else if (StringUtils.isAsciiNumberChar(x))
          !StringUtils.isAsciiNumberString(password.tail)
        else
          false
      }
    }
  }

  // include space
  val specialCharacters: Vector[Char] = """!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~""".toVector

  case object AlphabetNumberCharacterRule extends CharacterRule {
    def label(locale: Locale) = "英数字記号"
    def validate(password: String) = password.forall(c =>
      ('a' <= c && c <= 'z') ||
        ('A' <= c && c <= 'Z') ||
        ('0' <= c && c <= '9') ||
        specialCharacters.contains(c))
  }

  // exclude 
  val specialConservativeCharacters: Vector[Char] = """!"#$%'()*+,-./:;=>?@[\]^_`|}~""".toVector

  case object AlphabetNumberCharacterConservativeRule extends CharacterRule {
    def label(locale: Locale) = "英数字記号"
    def validate(password: String) = password.forall(c =>
      ('a' <= c && c <= 'z') ||
        ('A' <= c && c <= 'Z') ||
        ('0' <= c && c <= '9') ||
        specialConservativeCharacters.contains(c))
  }

  case object AllCharactersRule extends CharacterRule {
    def label(locale: Locale) = "英数字"
    def validate(password: String) = true
  }

  case object PasswordRepeatValidator extends RecordValidator {
    def validateRecord(p: Record): ValidationResult = {
      (p.getString(PROP_PASSWORD), p.getString(PROP_PASSWORD_REPEAT)) match {
        case (Some(pw), Some(pwr)) =>
          if (pw == pwr)
            Valid
          else
            ValueDomainFailure("パスワードが一致しません。再度ご入力ください。", pw)
        case (Some(pw), None) => Valid
        case (None, Some(_)) => Valid
        case (None, None) => Valid
      }
          
      
    }
  }
}
