/*
 * Copyright (c) 2014 h_sakurai. All rights reserved.
 *
 * RubyE.scala
 * RubyE Intepreter
 * This program is Tiny programming language Ruby like Expression processor.
 *
 * HISTORY:
 *  RubyE.scala prc,fun, append closure, quote, quasiquote, expand, macro, symbol,void
 * TODO:
 * match,for,string,define operator,comment,array(vector)
 * void,
 */

package rubye
import scala.util.matching.Regex
import scala.collection.mutable.{HashMap,Stack}

sealed trait E
case class ESym(a:String) extends E
case class EId(a:String) extends E
case class EInt(a:Int) extends E
case class EBin(a:E,b:E,c:E) extends E
case class EPost(a:E,b:E) extends E
case class EPre(a:E,b:E) extends E
case class EMsg(a:E,b:E,c:E,d:E) extends E
case class EPrn(a:E,b:E,c:E) extends E
case class EBlock(a:E,b:E,c:E,d:E,e:E) extends E

case object ENil extends E

object Parser {
	// rubye reader
	def parse(src:String):E = {
		var str = src
		var connectf = true
		val tokens = for(i <- Stream.from(0)) yield {
			def t ():E = {
				val num = """^([0-9]+)(.*$)""".r
				val sym = """^'([^-\+*\/\(\)\[\]\{\}\s\=\;\:<,]+)(.*$)""".r
				val ptn = """^([-\+*\/\=<>|:.]+|[\:\(\)\[\]\{\},]|[^-\+*\/\(\)\[\]\{\}\s\=\;\:<>|,.]+|$)(.*$)""".r
				val eol = """^(;)(.*$)""".r
				val spc1 = """^[\s]*[\r\n]+[\s]*(.*$)""".r
				val spc = """^[\s]+(.*$)""".r
				val eof = """^[\s]*($)""".r
				str match {
				case eol(a,e) => str = e; connectf = false; EId(a)
				case eof(e) => str = ""; ENil
				case sym(a,e) => str = e; ESym(a)
				case num(a,e) => str = e; EInt(a.toInt)
				case ptn(a,e) => str = e; EId(a)
				case spc1(e) => str = e; connectf = false; t()
				case spc(e) => str = e; t()
				case _ => throw new Error("lexer error")
				}
			}
			connectf = true
			t()
		}

		val eox:(E => Int) = {
			case EId(")") => 1
			case EId("}") => 1
			case EId("]") => 1
			case EId("is") => 1
			case EId("thn") => 1
			case EId("end") => 1
			case EId("lop") => 1
			case ENil => 1
			case _   => -1
		}

		val infixs:(E => Int) = {
			case EId("*") => 20
			case EId("/") => 20
			case EId("-") => 10
			case EId("+") => 10
			case EId("<=") => 6
			case EId(">=") => 6
			case EId("<") => 6
			case EId(">") => 6
			case EId("==") => 5
			case EId("..") => 4
			case EId("|") => 3
			case EId("=>") => 2
			case EId("in") => 2
			case _	 => -1
		}

		val infixrs:(E => Int) = {
			case EId(",") => 1
			case EId(":=") => 5
			case EId("thn") => 3
			case EId("els") => 3
			case EId("elf") => 3
			case _	 => -1
		}
		val infix2s:(E => Int) = {
			case _	 => -1
		}

		val prefixs:(E => Int) = {
			case EId("-") => 100
			case EId("reverse") => 3
			case EId("whn") => 1
			case _	 => -1
		}

		val postfixs:(E => Int) = {
			case EId("++") => 100
			case EId(";") => 0
			case _	 => -1
		}

		val parens:(E => Int) = {
			case EId("if") => 100
			case EId("(") => 100
			case EId("{") => 100
			case EId("[") => 100
			case _	 => -1
		}

		val endParens:(E => E) = {
			case EId("(") => EId(")")
			case EId("{") => EId("}")
			case EId("[") => EId("]")
			case EId("if") => EId("end")
			case _ => ENil
		}

		val sts:( (E, E) => Int ) = {
			case (EId("fun"),EId("(")) => 2
			case (EId("mac"),EId("(")) => 2
			case _ => -1
		}

		val adablock:(E => Int) = {
			case EId("fun") => 2
			case EId("prc") => 2
			case EId("cas") => 2
			case EId("for") => 2
			case EId("whl") => 2
			case _ => -1
		}
		val adablock2:(E => E) = {
			case EId("prc") => EId("is")
			case EId("fun") => EId("is")
			case EId("cas") => EId("is")
			case EId("for") => EId("lop")
			case EId("whl") => EId("lop")
			case a => throw new Error("undefined adablock "+ a)
		}
		sealed case class AS(a:E, s:Stream[E])
		val cons = Stream.cons
		def eat(t:E)(as:AS):AS = as.s match {
			case cons(x,xs) if(x==t) => AS(x, xs)
			case _ => throw new Error("error expected "+t+" but found "+as.s )
		}

		def exp(p:Int)(as:AS):AS = {
			//println("exp("+p+")("+as+")");
			as match {
			case AS(ENil, cons(x, xs))  if(eox(x)>0) => AS(EId("void"),cons(x,xs))
			case AS(ENil, cons(p1, xs)) if (p < parens(p1)) =>
				val AS(y, ys)  = exp(0)(AS(ENil, xs))
				val AS(p2, zs) = eat(endParens(p1))(AS(ENil, ys))
				exp(p)(AS(EBin(p1,y,p2),zs))

			case AS(ENil, cons(x, xs)) if (p < adablock(x)) =>
				val AS(y, ys) = exp(0)(AS(ENil, xs))
				val AS(z, zs) = eat(adablock2(x))( AS(ENil, ys) )
				val AS(a, as) = exp(0)(AS(ENil, zs))
				val AS(b, bs) = eat(EId("end"))( AS(ENil, as) )
				exp(p)(AS(EBlock(x, y, z, a, b), bs))

			case AS(ENil, cons(op, xs)) if (p < prefixs(op)) =>
				val AS(y, ys) = exp(prefixs(op))(AS(ENil, xs))
				exp(p)(AS(EPre(op,y),ys))
			case AS(ENil, cons(x, xs)) => exp(p)(AS(x, xs))
			case AS(x, cons(p1, xs)) if (0 < sts(x,p1)) =>
				val AS(y, ys)  = exp(0)(AS(ENil, xs))
				val AS(p2, zs) = eat(endParens(p1))(AS(ENil, ys))
				val AS(w, ws) = exp(sts(x,p1))(AS(ENil, zs))
				exp(p)(AS(EBlock(x,p1,y,p2,w), ws))
			case AS(x, cons(p1, xs))
					if (sts(x,p1) < 0 && p < parens(p1) && connectf) =>
				val AS(y, ys)  = exp(0)(AS(ENil, xs))
				val AS(p2, zs) = eat(endParens(p1))(AS(ENil, ys))
				exp(p)(AS(EMsg(x,p1,y,p2), zs))
			case AS(x, cons(op, xs)) if (p <= postfixs(op) && postfixs(op) == 0) =>
				val AS(y, ys) = exp(0)(AS(ENil, xs))
				if(y==ENil)AS(EPost(x,op),xs)
				else exp(0)(AS(EBin(EPost(x,op), EId("@"), y), ys))
			case AS(x, cons(op, xs)) if (p <= postfixs(op)) =>
				AS(EPost(x,op),xs)
			case AS(x, cons(op, xs)) if (p < infixs(op)) =>
				val AS(y, ys) = exp(infixs(op))(AS(ENil, xs))
				exp(p)(AS(EBin(x, op, y), ys))
			case AS(x, cons(op, xs)) if (p <=infixrs(op))=>
				val AS(y, ys) = exp(infixrs(op))(AS(ENil, xs))
				exp(p)(AS(EBin(x, op, y), ys))
			case AS(x, cons(op, xs)) if (p <=infix2s(op))=>
				val AS(y, ys) = exp(infixrs(op))(AS(ENil, xs))
				exp(p)(AS(EBin(x, op, y), ys))

			case AS(_, cons(x,xs)) if(eox(x)>0) => as
			case AS(ENil, xs) => as
			case AS(x, xs) if(xs == Nil) => as
			case AS(x, xs) if (p <= 0) =>
				val AS(y, ys) = exp(0)(AS(ENil, xs))
				if(y != ENil) exp(0)(AS(EBin(x, EId("@"), y), ys))
				else                 AS(x, xs)
			case as => as
			}
		}
		exp(0)(AS(ENil, tokens)).a
	}
}
object main {

	def main(argv:Array[String]) {
		println(Parser.parse(argv(0)))
		// println(eval(argv(0)))
	}

/*
	def eval(s:String):E = try {
		var env = new Env(ENil)
		env + ("macros" -> new Stack[(E,E)]())
		var exp = Macro.expand(parse(s),env)
		eval(exp,env)
	} catch {
	case e:Throwable => println(e);-1
	}

	class Env (val parent:Env) {
		var e:HashMap[E, E] = new HashMap
		def apply(a:E):E = {
			if (e.contains(a)) e(a)
			else if(parent != ENil)parent(a)
			else ENil
		}
		def contains(a:E) : Boolean = {
			if(e.contains(a)) true
			else if(parent == ENil) false
			else parent.contains(a)
		}
		def +(kv : (E, E)) : Env = {
			def add(env:Env, kv:(E, E)) : Boolean = {
				kv match {
				case (a, b) =>
					if(env.e.contains(a)) { env.e + kv; true }
					else (env.parent != ENil && add(env.parent, kv))
				}
			}
			if(!add(this, kv)) e + kv
			this
		}
		override def toString():String = {
			e.toString()+"\nparent:"+parent
		}
	}

	case class Fun(prms:E,body:E,e:Env) {
		override def  toString():String = {
			return "Fun("+prms+","+body+")"
		}
	}
	object Macro {
		def qq(a:E, e:Env):E = {
			a match {
			case (a,b) => (qq(a,e),qq(b,e))
			case (a,b,c) => (qq(a,e),qq(b,e),qq(c,e))
			case ("qe","{",b,"}") => b
			case ("q","{",b,"}") => a
			case ("uq","{",b,"}") => eval(b,e)
			case (a,b,c,d) => (qq(a,e),qq(b,e),qq(c,e),qq(d,e))
			case (a,b,c,d,f) => (qq(a,e),qq(b,e),qq(c,e),qq(d,e),qq(f,e))
			case a => a
			}
		}

		def expand(a:E, e:Env):E = {
			(a,e) match {
			case Macro(a,e) => a
			case ((a,b),e) => (expand(a,e),expand(b,e))
			case ((a,b,c),e) => (expand(a,e),expand(b,e),expand(c,e))
			case ((a,b,c,d),e) => (expand(a,e),expand(b,e),expand(c,e),expand(d,e))
			case (("mac","(",b,")",c),e) => e("macros").asInstanceOf[Stack[(E,E)]].push((b,c));0
			case ((a,b,c,d,f),e) => (expand(a,e),expand(b,e),expand(c,e),expand(d,e),expand(f,e))
			case (a,e) => a
			}
		}

		def unapply(arg:(E,Env)):Option[(E,Env)]=arg match {
		//case (("add","(",(b,",",c),")"),env) => Some((b,"+",c),env)
		case (a,e) =>
			for((prms,body) <- (e("macros").asInstanceOf[Stack[(E,E)]])) {
				var env = match5(prms,a,e)
				if(env != ENil) return Some(eval(body, env),e)
			}
			None
		}

		def match5(a:E,b:E,e:Env):Env = {
			var m = new Env(e)
			def mat(a:E,b:E):Boolean = {
				(a,b) match {
				case (Symbol(n),b) => m + (n -> b);true
				case ((a1,a2),(b1,b2)) => mat(a1,b1)&&mat(a2,b2)
				case ((a1,a2,a3),(b1,b2,b3)) => mat(a1,b1)&&mat(a2,b2)&&mat(a3,b3)
				case ((a1,a2,a3,a4),(b1,b2,b3,b4)) => mat(a1,b1)&&mat(a2,b2)&&mat(a3,b3)&&mat(a4,b4)
				case ((a1,a2,a3,a4,a5),(b1,b2,b3,b4,b5)) => mat(a1,b1)&&mat(a2,b2)&&mat(a3,b3)&&mat(a4,b4)&&mat(a5,b5)
				case (a,b) if(a==b)=> true
				case _ => false
				}
			}
			if(mat(a,b)) m
			else ENil
		}
	}

	def append(a:E,op:String, b:E):E = {
		a match {
		case (a, `op` ,as) => (a,op,append(as,op,b))
		case a => (a,op,b)
		}
	}

	// TODO: Return type change to Option.
	case object Exit extends Exception
	
	// ada expression evaluator
	def eval(a:E, e:Env):E = {
		//println("eval("+a+")");
		a match {
		case (a, ":=", b) => val r = eval(b,e);  e + (a -> r); r
		case (a, "=", b) => if(eval(a,e) == eval(b,e))-1 else 0
		case (a, "/=", b) => if(eval(a,e) != eval(b,e))-1 else 0
		
		case (a, ">", b) => (eval(a,e),eval(b,e)) match {
			case (a:Int, b:Int) => if(a > b) -1 else 0
			case (a1, b1) => throw new Error("unknown "+a +" > "+ b +" -> " + a1 + " > "+ b1)
			}

		case (a, "<", b) => (eval(a,e), eval(b,e)) match {
			case (a:Int, b:Int) => if(a < b) -1 else 0
			case (a1, b1) => throw new Error("unknown "+a +" < "+ b +" -> " + a1 + " < "+ b1)
			}

		case (a, "<=", b) => (eval(a,e),eval(b,e)) match {
			case (a:Int, b:Int) => if(a <= b) -1 else 0
			case (a1, b1) => throw new Error("unknown "+a +" <= "+ b +" -> " + a1 + " <= "+ b1)
			}
		case (a, ">=", b) => (eval(a,e),eval(b,e)) match {
			case (a:Int, b:Int) => if(a >= b) -1 else 0
			case (a1, b1) => throw new Error("unknown "+a +" >= "+ b +" -> " + a1 + " >= "+ b1)
			}
		case (a, "++") => val r = e(a); e + (a -> (r.asInstanceOf[Int] + 1)); r
		case (a, "@", b) => eval(a, e); eval(b, e)
		case ("Put", "(", a, ")") => val r = eval(a, e); println(r); r
		case ("evl", "(", b, ")") => eval(eval(Macro.expand(b,e),e),e)
		case (a, "(", b, ")") =>
			(eval(a, e), eval(b, e)) match {
			case (a:Int, b:Int) => a + b
			case (Fun(a, body, e), b) =>
				// arguments bind
				var e2 = new Env(e)
				def bind(a:E, b:E) {
					(a, b) match {
					case ((a,",",as),(b,",",bs)) => e2.e + (a -> b); bind(as, bs)
					case (a,b) => e2.e + (a -> b)
					}
				}
				bind(a, b)
				eval(body, e2)
			case (_,_) => throw new Error("unknown function:"+a)
			}
		case ("prc", a, "is", b, "end") => eval((a,"=",b), e)
		case ((a,"(",b,")"),"{",c,"}") => eval((a,"(",append(b,",",Fun("void",c,e)),")"), e)
		case ("q","{",b,"}") => b
		case ("qq","{",b,"}") => Macro.qq(b, e)
		case ("qqq","{",b,"}") => Macro.qq(eval(b,e), e)
		case (a,"{",b,"}") => eval(a, e).asInstanceOf[Int] * eval(b, e).asInstanceOf[Int]
		case (a,"[",b,"]") => eval(a, e).asInstanceOf[Int] - eval(b, e).asInstanceOf[Int]
		case ("(",a,")") => eval(a, e)
		case ("{",a,"}") => eval(a, e)
		case (a,"+",b) => eval(a, e).asInstanceOf[Int] + eval(b, e).asInstanceOf[Int]
		case (a,"*",b) => eval(a, e).asInstanceOf[Int] * eval(b, e).asInstanceOf[Int]
		case (a,"-",b) => eval(a, e).asInstanceOf[Int] - eval(b, e).asInstanceOf[Int]
		case (a,"/",b) => eval(a, e).asInstanceOf[Int] / eval(b, e).asInstanceOf[Int]
		case (a,",",b) => (eval(a, e),",",eval(b,e))
		case ("-",a)   => -(eval(a, e).asInstanceOf[Int])
		case (a, ";")  => eval(a, e)
		case ("if", b, "end") => eval(b, e)
		case ("cas", a, "is", b, "end") => val (ra, rb) = when_ (eval(a, e), b, e); rb
		case (a,"thn", (b,"els",c)) => if(eval(a,e) != 0) eval(b, e) else eval(c, e)
		case (a,"thn", (b,"elf",c)) => if(eval(a,e) != 0) eval(b, e) else eval(c, e)
		case (a,"thn", b) => if(eval(a,e) != 0) eval(b, e) else 0
		case ("fun", (a,  "(", p, ")" ) ,"is", b, "end") =>  val r = Fun(p,b,e);  e + (a -> r); r
		case ("fun", "(", p, ")",  b) =>  val r = Fun(p,b,e);  e + (a -> r); r
		case (a,"..",b) => (eval(a,e),"..",eval(b,e))
		case ("for", (name,"in", range), "lop", body, "end") if(eval(range, e)match{case (a,"..",b)=>true case _ => false}) =>
			var (a,_,b)=eval(range,e)

			var rc:E = 0
			try {
				var min = eval(a,e).toString.toInt
				var max = eval(b,e).toString.toInt
				if(min > max) {
					min = -min
					max = -max
					for(i <- min to max ) {
						e + (name -> -i)
						rc = eval(body,e)
					}
				} else {
					for(i <- min to max ) {
						e + (name -> i)
						rc = eval(body,e)
					}
				}
			} catch {
			case Exit =>
			}
			rc
		case ("reverse",(a,b,c)) => (c, b, a)
		case "ext" => throw Exit
		case ("whl", a, "lop", body, "end") =>
			var rc:E = 0
			try {
				while(eval(a ,e) != 0) {
					rc=eval(body, e)
				}
			} catch {
			case Exit =>
			}
			rc
		case a:Fun => a
		case a:String => e(a)
		case a:Symbol => a
		case a:Int => a
		case a => throw new Error("runtime error " + a)
	}}
	def dbg(a:E):E = {
		println("dbg "+a)
		a
	}

	def when_(a:E, b:E, e:Env):(Boolean, E) = {
		def whenOr(a:E, b:E, e:Env):Boolean = {
			b match {
			case (xs,"|",x) => whenOr(a,xs,e)|| (x==a)
			case x          => (x==a)
			}
		}
		b match {
		case ("whn", ("oth", "=>", y)) => (true, y)
		case ("whn", (x, "=>", y)) => if(whenOr(a,x,e))(true,eval(y,e)) else (false,ENil)
		case (x, "@", y) => val(ra, rb) = when_(a, x, e); if(ra) (ra,rb) else when_(a, y, e)
		case _ => throw new Error("error expected when but found "+b)
		}
	}
*/

}

