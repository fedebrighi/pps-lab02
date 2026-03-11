package task5

import java.util.function.Predicate

// overall module
object Optionals:

  // type "public" definition, exposing structure
  enum OptionalInt:
    case Just(value: Int)
    case Empty()

  // operations (/algorithms)
  object OptionalInt:

    def isEmpty(opt: OptionalInt): Boolean = opt match
      case Empty() => true
      case _       => false

    def orElse(opt: OptionalInt, orElse: Int): Int = opt match
      case Just(a) => a
      case _       => orElse

    def mapInt(opt: OptionalInt)(f: Int => Int): OptionalInt = opt match
      case Just(a) => Just(f(a))
      case _       => Empty()

    def filter(opt: OptionalInt) (predicate: Int => Boolean): OptionalInt = opt match {
      case Just(a) if predicate(a) => Just(a)
      case _                       => Empty()
    }

@main def tryOptionals: Unit =
  import Optionals.* // to work with Optionals (to see OptionalInt type)
  import OptionalInt.* // to directly access algorithms

  val s1: OptionalInt = Just(1)
  val s2: OptionalInt = Empty()

  println(s1) // Some(1)
  println(isEmpty(s1)) // false
  println(orElse(s1, 0)) // 1
  println(orElse(s2, 0)) // 0
  println(mapInt(s1)(i => i + 1))
  println(filter(Just(5))(_ > 2))
  println(filter(Just(5))(_ < 2))
