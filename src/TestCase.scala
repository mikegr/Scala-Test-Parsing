import scala.util.parsing.combinator._

object TestCase extends TestParser {
  def main(args:Array[String]):Unit =  {
    println(parseAll(term, "37+42"));
  }
}

class TestParser extends RegexParsers {

  def Numeral: Parser[String] =  """\d+""".r
  def term:Parser[Any] = Numeral ~ opt("+" ~ Numeral)
 
  
}