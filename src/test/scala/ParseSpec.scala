import org.scalatest.FlatSpec

class PaserSpec extends FlatSpec {
	import rubye._


	it should "parse int" in {
		assert(EInt(1)==Parser.parse("1"))
	}

	it should "parse infixs" in {
		assert(EBin(EInt(1),EId("+"),EInt(2))
			==Parser.parse("1+2"))

		assert(EBin(EBin(EInt(1),EId("+"),EInt(2)),EId("+"),EInt(3))
			==Parser.parse("1+2+3"))

		assert(EBin(EInt(1),EId("/"),EInt(2))
			==Parser.parse("1/2"))

		assert(EBin(EInt(1),EId("*"),EInt(2))
			==Parser.parse("1*2"))

		assert(EBin(EInt(1),EId("-"),EInt(2))
			==Parser.parse("1-2"))

		assert(EBin(EInt(1),EId("+"),EInt(2))
			==Parser.parse("1+2"))

		assert(EBin(EInt(1),EId("<="),EInt(2))
			==Parser.parse("1<=2"))

		assert(EBin(EInt(1),EId(">="),EInt(2))
			==Parser.parse("1>=2"))

		assert(EBin(EInt(1),EId(">"),EInt(2))
			==Parser.parse("1>2"))

		assert(EBin(EInt(1),EId("<"),EInt(2))
			==Parser.parse("1<2"))

		assert(EBin(EInt(1),EId("=="),EInt(2))
			==Parser.parse("1==2"))

		assert(EBin(EInt(1),EId(".."),EInt(2))
			==Parser.parse("1..2"))

		assert(EBin(EInt(1),EId("|"),EInt(2))
			==Parser.parse("1 | 2"))

		assert(EBin(EInt(1),EId("=>"),EInt(2))
			==Parser.parse("1 => 2"))

		assert(EBin(EInt(1),EId("in"),EInt(2))
			==Parser.parse("1 in 2"))
	}

	it should "parse infixrs" in {
		assert(EBin(EId("a"),EId(","),EInt(2))
			==Parser.parse("a,2"))

		assert(EBin(EId("a"),EId(":="),EInt(2))
			==Parser.parse("a:=2"))

		assert(EBin(EId("a"),EId("thn"),EInt(2))
			==Parser.parse("a thn 2"))

		assert(EBin(EId("a"),EId("els"),EInt(2))
			==Parser.parse("a els 2"))

		assert(EBin(EId("a"),EId("elf"),EInt(2))
			==Parser.parse("a elf 2"))
	}

	it should "parse := operator" in {
		assert(EBin(EId("a"),EId(":="),EInt(2))
			==Parser.parse("a:=2"))
	}
}

