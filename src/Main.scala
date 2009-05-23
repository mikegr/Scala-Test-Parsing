import scala.util.parsing.combinator._

object Main extends Combinatoric{

	def main(args:Array[String]) {
		
		
		println(parseAll(Prog, "begin(xy:=21*2;repeat(xy:=xy+1;xy:=xy-2)until(xy<0)taeper)end"));
		
		//println(parseAll(test, "34*56"));
		//println(parseAll(test, "34+887"));
		
		println(parseAll(Prog, "begin(a:=1;b:=a)end"));
		
	}
	
}
