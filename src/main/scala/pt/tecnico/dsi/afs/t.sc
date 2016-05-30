class Cenas(i: Int, s: String)

new Cenas(5, "simao")
//Cenas(5, "simao")  //Does not work because there is no companion object with an apply method


case class Cenas2(i: Int, s: String)
new Cenas2(5, "simao")
Cenas2(5, "simao")

object Cenas3 {
  def apply(i: Int, s: String) = new Cenas3(i, s)
}
class Cenas3(i: Int, s: String)

new Cenas3(5, "simao")
Cenas3(5, "simao")


object Cenas4 {
  def apply(s: String) = Cenas4(Right(Some(s)))
}
case class Cenas4(i: Either[Int, Option[String]])

new Cenas4(Right(Some("Simao")))
Cenas4(Right(Some("Simao")))
Cenas4("Simao")
Cenas4(Left(5))