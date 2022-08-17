object list {
  /**
   *
   * Реализовать односвязанный иммутабельный список List
   * Список имеет два случая:
   * Nil - пустой список
   * Cons - непустой, содердит первый элемент (голову) и хвост (оставшийся список)
   */

  trait List[+T] {
    val head: T
    val tail: List[T]

    def ::[TT >: T](elem: TT): List[TT] =
      List.::(elem, List.::(head, List.::(tail.head, tail.tail)))

    def mkString(separator: String = ", "): String = {
      if (this == List.Nil)
        return ""

      var list: List[T] = this
      var res = ""
      do {
        res += s"$head$separator"
        list = tail
      } while (tail != List.Nil)
      res
    }
  }

  object List {
    case class ::[A](head: A, tail: List[A]) extends List[A]

    case object Nil extends List[Nothing] {
      override val head: Nothing = NullPointerException
      override val tail: List[Nothing] = throw NullPointerException
    }


    def apply[A](v: A*): List[A] = if (v.isEmpty) List.Nil
    else ::(v.head, apply(v.tail: _*))
  }
}
// printIfAny
// zip
//

val a = List.::(5, List.Nil).::(3).::(2)
println(a.mkString(" i said! "))