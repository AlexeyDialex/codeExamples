package cats.examples

trait Printable[A] {
  import Printable._
  def format(value: A): String
}
object Printable {
  def format[A](input: A)(implicit p: Printable[A]): String =
    p.format(input)

  def print[A](input: A)(implicit p: Printable[A]): Unit =
    println(format(input))
}
