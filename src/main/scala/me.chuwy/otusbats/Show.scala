package me.chuwy.otusbats

// 1 type constructor
trait Show[A] {
  def show(a: A): String
}

object Show {

  // 1.1 Instances (`Int`, `String`, `Boolean`)
  implicit val showInt: Show[Int] = fromFunction[Int](a => a.toString)
  implicit val showString: Show[String] = fromJvm
  implicit val showBoolean: Show[Boolean] = fromFunction[Boolean](b => b.toString)

  // 1.2 Instances with conditional implicit
  implicit def listShow[A](implicit ev: Show[A]): Show[List[A]] = (a: List[A]) => a.map(ev.show).mkString(",")


  // 2. Summoner (apply)
  def apply[T](implicit show: Show[T]): Show[T] = show


  // 3. Syntax extensions

  implicit class ShowOps[A](a: A) {
    def show(implicit ev: Show[A]): String = ev.show(a)

    def mkString_[B](begin: String, end: String, separator: String)(implicit S: Show[B], ev: A <:< List[B]): String = {
      // with `<:<` evidence `isInstanceOf` is safe!
      val casted: List[B] = a.asInstanceOf[List[B]]
      Show.mkString_(casted, separator, begin, end)
    }

  }

  /** Transform list of `A` into `String` with custom separator, beginning and ending.
   *  For example: "[a, b, c]" from `List("a", "b", "c")`
   *
   *  @param separator. ',' in above example
   *  @param begin. '[' in above example
   *  @param end. ']' in above example
   */
  def mkString_[A: Show](list: List[A], begin: String, end: String, separator: String): String =
    list.mkString(begin, separator, end)


  // 4. Helper constructors

  /** Just use JVM `toString` implementation, available on every object */
  def fromJvm[A]: Show[A] = new Show[A] {
    override def show(a: A): String = a.toString
  }

  /** Provide a custom function to avoid `new Show { ... }` machinery */
  def fromFunction[A](f: A => String): Show[A] = new Show[A] {
    override def show(a: A): String = f(a)
  }

}
