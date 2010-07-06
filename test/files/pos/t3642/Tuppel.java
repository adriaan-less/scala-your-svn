public class Tuppel<T> {
  private Tuppel( T values ) {}

  public static <A> Tuppel<A> get() {
    return new Tuppel<A>(null) {};
  }
}