package cats.examples

object PrintableInstances {
  implicit val stringPrintable: Printable[String] = new Printable[String] {
    def format(input: String): String = input
  }

  implicit val intPrintable: Printable[Int] = new Printable[Int] {
    def format(input: Int): String = input.toString
  }
}
