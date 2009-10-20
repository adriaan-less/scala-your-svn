object Bug {
  abstract class FChain {
    type T

    def chain(constant:String) =
      new FConstant[this.type](constant, this) //removing [this.type], everything compiles
  }

  case class FConstant[E <: FChain](constant:String, tail:E) extends FChain {
    type T = tail.T
  }
  
  object FNil extends FChain {
    type T = Unit
  }

  // this works
  // val a1 = FNil.chain("a").chain("a")
  // val a2 = a1.chain("a")

  // this gives the error:
  /*
 found   : Bug.FConstant[_2.type(in value a)] where type _2.type(in value a) <: Bug.FConstant[_1.type] with Singleton
 required: Bug.FConstant[_2.type(in value a)] forSome { type _2.type(in value a) <: Bug.FConstant[_1.type] with Singleton; val _1: Bug.FConstant[Bug.FNil.type] }
  val a = FNil.chain("a").chain("a").chain("a")
      ^  
  */
  val a = FNil.chain("a").chain("a").chain("a")
}