package it.unibo.pps.u02

object Lab2 extends App {

  // TUTTO IL LABORATORIO LO HO SVOLTO DA SOLO

  // TASK PART 1:

  def mult(x: Int, y: Int): Int = x * y

  def curriedCubed(x: Int): Int = x * x * x

  def curriedDivide(x: Double)(y: Double): Double = x / y

  enum Person:
    case Student(name: String, surname: String, year: Int)
    case Teacher(name: String, surname: String, year: Int)

  val p: Person = Person.Student("Federico", "Brighi", 2026)

  def surname(p: Person): String = p match
    case Person.Student(_, s, _) => s
    case Person.Teacher(_, s, _) => s

  println(" TASK 1 ")
  println(surname(p))


  //TASK PART 2:

  // 3a)

  val function: Int => String = _ match
    case n if n >= 0 => "positive"
    case _ => "negative"

  val function: Int => String = {
    case n if n >= 0 => "positive"
    case _ => "negative"
  }

  def function(n: Int): String = n match
    case n if n >= 0 => "positive"
    case _ => "negative"

  // 3b)

  val neg: (String => Boolean) => (String => Boolean) = p => (s: String) => !p(s)

  def neg(p: String => Boolean): String => Boolean = s => !p(s)

  // 4)

  val funType: (Double, Double, Double) => Boolean = (x: Double, y: Double, z: Double) => (x, y, z) match
    case _ if (x <= y && y == z) => true
    case _ => false

  val funType: Double => Double => Double => Boolean = x => y => z => (x, y, z) match
    case _ if (x <= y && y == z) => true
    case _ => false

  def funType(x: Double, y: Double, z: Double): Boolean = (x, y, z) match
    case _ if (x <= y && y == z) => true
    case _ => false

  def funType(x: Double)(y: Double)(z: Double): Boolean = (x, y, z) match
    case _ if (x <= y && y == z) => true
    case _ => false

  // 5)

  def compose(f: Int => Int, g: Int => Int): Int => Int = param => f(g(param))
    (prima applichi g al parametro e poi a quel risultato gli applichi f)


}
