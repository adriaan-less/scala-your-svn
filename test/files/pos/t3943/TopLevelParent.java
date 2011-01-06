package test;

public abstract class TopLevelParent<T>{
  public class Inner { }
}

abstract class Child<C> extends TopLevelParent<C> {
  abstract void f(Inner ic);
}