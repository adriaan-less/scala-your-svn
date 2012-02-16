object Junk {
  def f(l: List[String]): List[String] = {
    val ret = l.map({ case MyMatch(id) => id })
    val MyMatch = "(\\d+)".r
    ret
  }
}
