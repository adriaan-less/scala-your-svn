import scala.collection.immutable._

object Test extends Application {
 val res0 = TreeSet(1, 2, 3)

 // res0.map(x => x)(TreeSet.newBuilder[Int])

 res0.map(x => x)(TreeSet.newBuilder)
 // passing in a builder where a canbuildfrom is expected
 // newBuilder's type param cannot be determined,
 // which causes the implicit search for its Ordering context bound to fail
}
