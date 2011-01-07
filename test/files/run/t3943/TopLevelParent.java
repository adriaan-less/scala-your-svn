public abstract class TopLevelParent<T> {
  public class Inner { }
}

abstract class Child<C> extends TopLevelParent<C> {
  abstract void f(Inner ic); // the implicit prefix for Inner is TopLevelParent<T> instead of TopLevelParent<C>
}