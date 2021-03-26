//----------------------------------------------------------------------
//case class Student(var name:String = "Sourya", val roll:Int = 24) {
//  //  def show() = println("hello beautiful")
//  def >(s2: Student) = roll > s2.roll;
//
//}
//  var s1 = Student()
//  var s2 = Student("Dharmendra", 11);
//  s1 > s2;
//---------------------------------------------------------------------
//var nums : Array[String]  = new Array[String]
//for(n<- nums)
//  {
//    println(n)
//  }
//nums.foreach(i => print(i+" "))
//var newList = nums.reverse;  // reversing list
// obj.reverse return new list (list obj ref)
//newList.foreach{ i : Int => println(i)}
//var list = newList.drop(2).take(2)
//list
//-----------------------------------------------------------------
//case class testObj() {
//  def max(x:Int, y:Int) = {
//    if(x > y) x
//    else y
////  return x > y ? x : y;
//  }
////}
////var n = new testObj()
////var c =max(6,10)
////print(c)
//max(3,5)
////----------------------
//def greet() = "hello"
//
//greet()


//----------------------------------------------------------------
//class Foo (){
//   val bar = "Bar"
//}
//class Too () extends Foo {
//
//  def method() = {
//    val f = new Foo
//    print(f.bar)
//  }
//}
//val t = new Too
//t.method()
//class Foo {
//  private val bar = "Bar"
//  def show(foo: Foo){
//    print("Show " + foo.bar)
//  }
//}
//val v = new Foo()
//v.show(new Foo())

//object Hello{
//  val msg = "Hello World!"
//  val msg2 ="Good bye"
//  def method() = println(msg + " "+ msg2)
//}
//Hello.method()
//---------------------------------------------------------
//class Rational(n: Int, d: Int) {
//  import Rational.gcd
// private val g: Int = gcd(n.abs, d.abs)
//  val numer: Int = n/g
//  val denom: Int = d/g
//  override def toString = n+"/"+d
//}
//object Rational {
//  private def gcd(a: Int, b: Int): Int = if (b==0) a else gcd(b, a%b)
//}
//val rat = new Rational(44,33)
//rat.numer
//rat.denom

//----------------------------------------------------
//object  Foo {
//  def isValidAge(age:Int) = {
//    age <= MAX_AGE && age >= MIN_AGE;
//  }
//}
//object foo {
//  val MAX_AGE = 60
//  val MIN_AGE = 18
//}
//


//--------------------------------------------------------------

//
//class Rational(n: Int, d: Int) {
//  import Rational.gcd
//private val g: Int = gcd(n.abs, d.abs)
//  val numer: Int = n/g
//  val denom: Int = d/g
//  override def toString = n+"/"+d
//}
//object Rational {
//  private def gcd(a: Int, b: Int): Int = if (b==0) a else gcd(b, a%b)
//}
//val r = new Rational(55,66)
//

//-------------------------------------------------------------------
//------Rational Class ----------------------------

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
val rat = new Rational(77,66)
//--------------------------------------------------------------------
val str = "abc"
str.hashCode
val str1 = "abc"
str1.hashCode
//----------------------------------------------------------

def longestWord(words: Array[String]):(String,Int) = {
  var word = words(0)
  var idx = 0
  for (i <- 1 until words.length)
    if (words(i).length > word.length) {
      word = words(i)
      idx = i
    }
  (word, idx)
}
longestWord(Array[String]("hello","sam fisher","zack synder","ned startk","jon snow"))
val longest = longestWord("The quick brown fox".split(" "))
val (word, idx) = longest
//---------------------------------------

val m = Map(1 -> "Rob", 2-> "Ned", 3 -> "Jon")
m(3)


val l1 = List(1,2,3,4,5)
val l2 = Set(6,7,8,9)
val l = l1 ++ l2
val l3 = 10 :: l

val i = IndexedSeq(1,2,3,4,5,6)
val i1 = i :+ 10

//---------------------------------------------------

val m = Map(1 -> "Rob", 2-> "Ned", 3 -> "Jon")
//m.getOrElse(1)
println(m)

//------------------------------------------------------


val num = List(1,2,3)
num.map((n:Int) =>n*n)

num map(_ * 9)

val addOne = (x:Int)=> x*10

val num1 = num map addOne

//----------------------
val addOne: (Int => Int) = x => x + 1
addOne(3)

//-----------------------------------------------------

val l = List(1,2,3,4,5,6)
def addOne(x: Int) = x + 1
val f = addOne _
//val m = l filter((p:Int)=>p% 2 == 0)
//val m = l filter(p=>p% 2 == 0)
val m = l filter(_ % 2 == 1)

val mp = l map (_ + 10)
val mp = l map ((p:Int) => p + 10)
val mp = l map (p=> p + 10)
val sqm : Int => Int = x => x * x;
val sq = l.map(sqm)


//------------------------------------------------------------
val l = List(1,2,3,4,5)

val nl = l.flatMap(n=> List.fill(n)(n))

//--------------------------------------------------------

val isEven: PartialFunction[Int, String] = {case x if x % 2 == 0 => x + " is even"}

isEven.isDefinedAt(9)
//-------------------------------------------------------

val l = List(1,2,3,4,5)
//val e = l filter(n=> n % 2 == 0)
//
//val sq = e map(n=>n*n)

val eq = l.collect{case n if (n % 2 == 0) => n * n}


//---------------------------------------------------------------

import scala.io.Source

object Hello {
  def main(args: Array[String]) = {
    if (args.length > 0) {
      for (line <- Source.fromFile(args(0)).getLines())
        println(line.length + " " + line)
    }
  }
}
Hello.main(Array("Hello.scala"))

//----------------------------------------------------------------

def myFun() = {
  for{
    i <- 1 to 3 if i % 2 == 1
    j <- 1 to i
  } yield i * j
}
val l = myFun();

//---------------------------------------------------------------

val list = List(1,2,3,4,5)

val SquareAndRemoveOdd = for {
  i <- list if i % 2 == 0
  ele = i * i
} yield {
  ele
}
//--------------------------------------------------------------------
val v = for (i <- 1 to 3) yield i+1
print(v)

val l = 1 to 3 map ( i=> i + i)

print(for(i <- 1 to 3; j <- 1 to i) yield i * j)

1 to 3 flatMap(i=>1 to i map(j=>i * j))

//---------------------------------------------------------------
for (i <- 1 to 10) print(i+" ")

1 to 3 foreach(i=> print(i+" "))


//-------------------------------------------

def sum (a:Int, b:Int) = {
  def sum1(x:Int, y:Int) = {
    x + y
  }
  sum1(a+5,b)
}
val l = sum(2,3)

//----------------------------------------------------------------


def fact(n: Long): Long = {
  if (n == 1) 1 else n * fact(n - 1)
}

def factorial (n:Int):Int = {
  def factorial (n: Int, acc:Int):Int = {
    if(n == 1) acc
    else factorial(n-1, acc * n)
  }
  factorial(n, 1)
}
factorial(9)


//-----------------------------------------------------------------

// finding max using recursion

def max(list : List[Int]) : Int = {
  def max(list:List[Int], acc:Int, index : Int):Int = {
    if(list.size == index) {
      acc
    }
    else {
      if(acc < list(index)) max(list, list(index), index + 1)
      else max(list, acc, index+1)
    }
  }
  max(list, -1, 0)
}

val list = List(1,21,3,4,5,6,7)

max(list)

//------------------------------------------------------------------
def max(list : List[Int]) : Int = {
  def max(list:List[Int], acc:Int, index : Int):Int = {
    if(list.size == index) {
      acc
    }
    else {
      if(acc < list(index)) max(list, list(index), index + 1)
      else max(list, acc, index+1)
    }
  }
  max(list, -1, 0)
}

val list = List(1,21,3,4,5,6,7)

max(list)

//--------------------------------------------------------------------


def sum (a:Int, b:Int, c:Int) = {
  a + b + c
}

val b = sum(1, _:Int, 4)
val c = b(5)
print(c)

//--------------------------------------------------------------
def sum(x:Int, y:Int) = {
  x+y
}
def sub(x:Int, y:Int) = {
  x-y
}
def sumSub(f:((Int, Int)=>Int), x:Int, y:Int) = {
  print(f(x,y))
}

sumSub(sum, 1,2);
//-----------------------------------------------------------

def curriedSum(x: Int)(y: Int)(z: Int)(w:Int) = x + y + z + z
val one = curriedSum(10) _
val two = one(20)
val tr = two(30)
tr(50)
curriedSum(10)(20)(30){40}
//--------------------------------------------------------------

val list =List(1,2,3)

val a = list.fold(0){(acc,ele)=> acc+ele}
val b = list.foldLeft(0){(acc,ele)=> acc+ele}
val c = list.foldRight(0){(acc,ele)=> acc+ele}


//----------------------------------------------------------------
val list = List(1,2,3,4)

def reverse(list:List[Int]):List[Int] = {
  list.foldLeft(List[Int]()){(acc,ele)=>
    (ele::acc)
  }
}
reverse(list)


