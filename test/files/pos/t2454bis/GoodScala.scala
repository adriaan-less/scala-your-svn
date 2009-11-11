class GoodScala extends EvilRaw {
  override def createMonad: Monad[_] = error("foo")
}