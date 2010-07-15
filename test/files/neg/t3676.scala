import scala.collection._
import scala.collection.generic._

trait MyLike[A] extends Seq[A] with GenericTraversableTemplate[A, MyColl] with SeqLike[A, MyColl[A]] {
   override def companion = null 
}

trait MyColl[A] extends MyLike[A] 
object MyColl extends SeqFactory[MyColl]

