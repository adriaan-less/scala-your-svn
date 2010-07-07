public class Tuppel<T> {
  private Tuppel(){}

  public static <A> Tuppel<A> get() {
    return new Tuppel<A>() {};
  }
}