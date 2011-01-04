object Test {
  // bug only triggers if both these methods are named exactly like this and neither has a return type
  // rename _1 to _a1 or similar for _2 and it works, similar for adding the result type
  // is symbol resolution considering the method `_1` for the named argument `_1` ??
  def _1 = (0, 0) copy (_1 = 1)
  def _2 = (0, 0) copy (_2 = 2)
}