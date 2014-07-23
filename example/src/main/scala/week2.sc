object week3 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

	val x = new Rational(1,3)                 //> x  : Rational = 1/3

	x.neg                                     //> res0: Rational = 1/-3
	
	val y = new Rational(5,7)                 //> y  : Rational = 5/7
	
	y                                         //> res1: Rational = 5/7
	
	val z = new Rational(3,2)                 //> z  : Rational = 3/2
	
	x.sub(y).sub(z)                           //> res2: Rational = -79/42
	
}



class Rational(x: Int, y: Int){
	private def gcd(a: Int, b: Int): Int = if(b==0) a else gcd(b, a%b)
	private val g = gcd(x,y)
	
	def number = x / g
	def denom = y / g
	
	def add(that: Rational) =
		new Rational(number*that.denom+that.number*denom,denom*that.denom)
		
	override def toString = number + "/" + denom

	def neg =
		new Rational(-number,denom)
		
	def sub(that: Rational) = add(that.neg)
}