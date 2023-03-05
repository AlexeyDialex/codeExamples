package cats.examples

object PrintableSyntax{
  implicit class PrintableOps[A](value: A) {
    def niceFormat(implicit pr: Printable[A]): String = {
      pr.format(value)
    }

    def nicePrint(implicit pr: Printable[A]): Unit = {
      println(s"nice print: ${pr.format(value)}")
    }
  }
}

