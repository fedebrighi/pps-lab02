package it.unibo.pps.u02

import task5.Optionals.OptionalInt
import task5.Optionals.OptionalInt.{Empty, Just, filter, mapInt}

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


  //TASK PART 2:

  // 3a)

  val functionV1: Int => String = _ match
    case n if n >= 0 => "positive"
    case _ => "negative"

  def functionV2(n: Int): String = n match
    case n if n >= 0 => "positive"
    case _ => "negative"

  println("TEST 3a")
  println(functionV1(5)) // positive
  println(functionV2(-3)) // negative
  println()

  // 3b)

  val negVal: (String => Boolean) => String => Boolean = p => (s: String) => !p(s)

  def negDef(p: String => Boolean): String => Boolean = s => !p(s)

  println("TEST 3b")
  val empty: String => Boolean = _ == ""
  val notEmpty = negDef(empty)
  println(notEmpty("foo")) // true
  println(notEmpty("")) // false
  println(notEmpty("foo") && !notEmpty("")) // true
  println()

  // 4)

  val funTypeV1: (Double, Double, Double) => Boolean = (x: Double, y: Double, z: Double) => x <= y && y == z

  val funTypeV2: Double => Double => Double => Boolean = x => y => z => x <= y && y == z

  def funTypeV3(x: Double, y: Double, z: Double): Boolean = x <= y && y == z

  def funTypeV4(x: Double)(y: Double)(z: Double): Boolean = x <= y && y == z

  println("TEST 4")
  println(funTypeV1(1, 2, 2)) // true
  println(funTypeV1(3, 2, 2)) // false
  println(funTypeV2(1)(2)(2)) // true
  println(funTypeV3(1, 2, 2)) // true
  println(funTypeV4(1)(2)(2)) // true
  println()

  // 5)

  def compose(f: Int => Int, g: Int => Int): Int => Int = param => f(g(param))

  println("TEST 5")
  println(compose(_ - 1, _ * 2)(5)) // 9
  println(compose(_ + 1, _ * 3)(4)) // 13
  println()

  // TASK PART 3:

  // 7)

  def powerV1(base: Double, exponent: Int): Double = (base, exponent) match
    case (0, _) => 0.0
    case (1, _) => 1.0
    case (_, 0) => 1.0
    case (_, exp) if exp < 0 => 0.0
    case (base, exp) => base * powerV1(base, exp - 1)

  def powerV2(base: Double, exponent: Int): Double =
    @annotation.tailrec
    def _pow(b: Double, e: Int, acc: Double): Double = (b, e) match
      case (0, _) => 0.0
      case (1, _) => 1.0
      case (_, 0) => acc
      case (_, exp) if exp < 0 => 0.0
      case (b, e) => _pow(b, e - 1, acc * b)
    _pow(base, exponent, 1)

  println("TEST 7")
  println(powerV1(2, 3)) // 8.0
  println(powerV1(5, 2)) // 25.0
  println(powerV2(2, 3)) // 8.0
  println(powerV2(5, 2)) // 25.0
  println()

  // 8)

  def reverseNumber(n: Int): Int =
    @annotation.tailrec
    def _reverse(n: Int, acc: Int): Int = n match
      case _ if n <= 0 => acc
      case remainder => val digit = remainder % 10
        _reverse(remainder / 10, acc * 10 + digit)
    _reverse(n, 0)

  println("TEST 8 REVERSE")
  println(reverseNumber(12345)) // 54321
  println()

  // TASK PART 4

  // 8)

  enum Expr:
    case Literal(constant: Int)
    case Add(expr1: Expr, expr2: Expr)
    case Multiply(expr1: Expr, expr2: Expr)

  object Module8:
    def evaluate(expr: Expr): Int = expr match
      case Expr.Literal(c) => c
      case Expr.Add(e1, e2) => evaluate(e1) + evaluate(e2)
      case Expr.Multiply(e1, e2) => evaluate(e1) * evaluate(e2)

    def show(expr: Expr): String = expr match
      case Expr.Literal(c) => c.toString
      case Expr.Add(e1, e2) => "(" + show(e1) + "+" + show(e2) + ")"
      case Expr.Multiply(e1, e2) => "(" + show(e1) + "*" + show(e2) + ")"

  println("TEST 8 EXPR")
  println(Module8.evaluate(Expr.Add(Expr.Literal(2), Expr.Literal(3)))) // 5
  println(Module8.evaluate(Expr.Multiply(Expr.Literal(3), Expr.Literal(4)))) // 12
  println(Module8.show(Expr.Add(Expr.Literal(2), Expr.Literal(3)))) // (2+3)
  println(Module8.show(Expr.Multiply(Expr.Literal(3), Expr.Literal(4)))) // (3*4)
  println()

  // TASK 5

  // 9)

  def mapInt(opt: OptionalInt)(f: Int => Int): OptionalInt = opt match
    case Just(a) => Just(f(a))
    case _ => Empty()

  def filter(opt: OptionalInt)(predicate: Int => Boolean): OptionalInt = opt match {
    case Just(a) if predicate(a) => Just(a)
    case _ => Empty()
  }

  println("TEST 9")
  val s1: OptionalInt = Just(1)
  println(mapInt(s1)(i => i + 1))
  println(filter(s1)(_ > 2))
  println(filter(s1)(_ < 2))
}
