import org.scalatest.FlatSpec

class PaserSpec extends FlatSpec {
	import rubye._


	it should "parse int" in {
		assert(EInt(1)==Parser.parse("1"))
	}

	it should "parse bin operator" in {
		assert(EBin(EInt(1),EId("+"),EInt(2))==Parser.parse("1+2"))
		assert(EBin(EBin(EInt(1),EId("+"),EInt(2)),EId("+"),EInt(3))
			==Parser.parse("1+2+3"))
	}

	it should "parse bin operator2" in {
		assert(EBin(EInt(1),EId("+"),EInt(2))==Parser.parse("1+2"))
		assert(EBin(EBin(EInt(1),EId("+"),EInt(2)),EId("+"),EInt(3))
			==Parser.parse("1+2+3"))
	}
}

