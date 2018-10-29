package fpinscala.applicative

import java.util.Date

case class WebForm(name: String, birthdate: Date, phoneNumber: String)

object Validator extends App {
  def validateName(name: String): Validate[String] =
    if (name != "") ValidateSuccess(name)
    else ValidateFailure("Name cannot be empty" :: Nil)

  def validateBirthdate(birthdate: String): Validate[Date] = try {
    import java.text._
    ValidateSuccess(new SimpleDateFormat("yyyy-MM-dd").parse(birthdate))
  } catch {
    case _: Throwable => ValidateFailure("Birthdate must be in the form yyyy-MM-dd" :: Nil)
  }

  def validatePhone(phoneNumber: String): Validate[String] = {
    if (phoneNumber.matches("[0-9]{10}"))
      ValidateSuccess(phoneNumber)
    else ValidateFailure("Phone number must be 10 digits" :: Nil)
  }

  import Applicative._
  def validateWebFormMonad(firstName: String, lastName: String, dob: String, phone: String): Validate[WebForm] =
    for {
      fName <- validateName(firstName)
      lName <- validateName(lastName)
      d <- validateBirthdate(dob)
      number <- validatePhone(phone)
    } yield WebForm(fName + lName, d, number)

  def validateWebFormApplicative(firstName: String, lastName: String, dob: String, phone: String): Validate[WebForm] =
    (WebForm.apply _).curried.pure
        .<*>(validateName(firstName))
        .<*>(validateBirthdate(dob))
        .<*>(validatePhone(phone))

  println(validateWebFormMonad("", "", "1989-01-01", "1234567890"))
  println(validateWebFormMonad("Vu", "Ho", "1989-01", "123456790"))
  println(validateWebFormApplicative("Vu", "Ho", "1989-01", "23456790"))
}
