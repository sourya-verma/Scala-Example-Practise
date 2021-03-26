object Rational { 
		 def gcd(a: Int, b: Int): Int = if (b==0) a else gcd(b, a%b)
}

class Rational(n: Int, d: Int) {
	import Rational.gcd
	 val g: Int = Ratioanl.gcd(n.abs, d.abs)
	val numer: Int = n/g 
	val denom: Int = d/g 
	override def toString = n+"/"+d
}
