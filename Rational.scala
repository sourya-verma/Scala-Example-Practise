class Rational(n: Int, d: Int = 1) {
	val numer: Int = n
	val denom: Int = d
	require(d != 0 ,"denom must be non zero integer")
	def this(n:Int) = this(n,1)

	// import Rational.gcd
  	import Rational.gcd
	private val g: Int = gcd(n.abs, d.abs)


	def +(rational: Rational): Rational ={
		val newNumer = numer*rational.denom+rational.numer*denom
		val newDenom = denom*rational.denom
		new Rational(newNumer/g,newDenom/g)
	}
	def -(rational: Rational): Rational ={
		val newNumer = numer*rational.denom-rational.numer*denom
		val newDenom = denom*rational.denom
		new Rational(newNumer/g,newDenom/g)
	}
	def *(rational: Rational): Rational ={
		val newNumer = numer*rational.numer
		val newDenom = rational.denom*denom
		new Rational(newNumer/g,newDenom/g)
	}

	def /(rational: Rational): Rational ={
		val newNumer = numer*rational.denom
		val newDenom = rational.numer*denom
		new Rational(newNumer/g,newDenom/g)
	}
	def <(rational: Rational):Boolean = {
		this.numer*rational.denom < rational.numer*this.denom
	}
	def max(rational: Rational):Rational = {
		if (this <(rational)) rational 
		else this
	}
	
	def + (i:Int):Rational = {
		val newNumer = denom*i+numer
		val newDenom =  denom
		new Rational(newNumer/g,newDenom/g)
	}
	def - (i:Int):Rational = {
		val newNumer = numer - denom*i
		val newDenom =  denom
		new Rational(newNumer/g,newDenom/g)
		
	}
	def * (i:Int):Rational = {
		val newNumer = numer*i
		val newDenom = denom
		new Rational(newNumer/g,newDenom/g)

	}
	def / (i:Int):Rational = {
		val newNumer = numer
		val newDenom = denom*i
		new Rational(newNumer/g,newDenom/g)
	}
	
	
	override def toString = n+"/"+d
}

object Rational {
  private def gcd(a: Int, b: Int): Int = if (b==0) a else gcd(b, a%b)
}










