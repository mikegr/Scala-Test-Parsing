import scala.util.parsing.combinator._

class Combinatoric extends RegexParsers {

	def Prog: Parser[Any] = "begin" ~> Stmt <~ "end" ^^ ("BeginSymb" :: _ :: "EndSymb" :: Nil) 
	
	def Stmt: Parser[Any] = AssStmt | IfStmt | RepeatStmt | CompStmt 
		
	def AssStmt: Parser[Any] = Idf ~ ":=" ~ Aexpr ^^ {case x~y~z => x :: "AssOp" :: z :: Nil}
	
	def IfStmt: Parser[Any] = "if" ~ Bexpr ~ "then" ~ Stmt ~ "else" ~ Stmt ~ "fi" ^^ {case a~b~c~d~e~f~g => "IfSymb" :: b :: "ThenSymb" :: d :: "ElseSym" :: f :: "FiSymb" :: Nil}
		
	def RepeatStmt: Parser[Any] = "repeat" ~ Stmt ~ "until" ~ Bexpr ~ "taeper" ^^ {case a~b~c~d~e => "RepeatSym" :: b :: "UntilSym" :: d :: "TaeperSymb" :: Nil}
		
	def CompStmt: Parser[Any] = "(" ~>  Stmt ~ ";" ~ Stmt <~ ")" ^^ {case x~y~z => x :: "SemicolonSymb" :: z :: Nil}
		
	//def Expr:Parser[Any] = Aexpr | Bexpr  
	
	def Aexpr: Parser[Any] = Term | (Aexpr ~ Aop ~ Term)    
	
	def Term: Parser[Any] = Factor | (Term ~ Mop ~ Factor)   
	
	def Factor: Parser[Any] = Opd | ( "(" ~  Aexpr ~ ")" )   
	def Opd: Parser[Any] =   Idf | Numeral  
	def Aop: Parser[Any] = ("+" | "-") ^^ {case "+" => "Plus"; case "-" => "Minus"}
	def Mop: Parser[Any] = ("*" | "/") ^^ {case "*" => "Mult"; case "/" => "Div"}
	
	def Bexpr: Parser[Any] = ( "(" ~>  Aexpr ~ Relop ~ Aexpr <~ ")" ) ^^ ("LeftParenth"  :: _  :: "RightParenth" :: Nil)  
	def Relop: Parser[Any] = equalOp | unEqualOp | greaterOp | lessOp  
	
	def equalOp:Parser[Any] = "=" ^^ (_=>"Equal") 
	def unEqualOp :Parser[Any] =  "/=" ^^ (_=>"Unequal")  
	def greaterOp:Parser[Any] = ">" ^^ (_=>"Greater") 
	def lessOp :Parser[Any] =  "<" ^^ (_=>"Less")
	
	def Numeral: Parser[String] = 	"""(\d+)""".r ^^ (_ => "Num") 
	def Idf: Parser[String] = """[a-zA-Z][a-zA-Z0-9]*""".r ^^ (_ =>"Id")
	
	
}
