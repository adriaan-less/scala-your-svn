// motivating example for currying of type parameter lists
// from our oopsla 2010 paper on implicits

object Sessions {
  trait Session {
    type Dual <: Session

    def run(dp: Dual): Unit
  }

  sealed case class Stop extends Session {
    type Dual = Stop

    def run(dp: Dual): Unit = {}
  }

  // typevars from later parameter lists may occur on other side of <: from typevar in earlier list
  // 
  sealed case class In[A, B <: Session{type Dual = BDual}][BDual <: Session](recv: A => B) extends Session {
    type Dual = Out[A, BDual] // type inference provides second list

    def run(dp: Dual): Unit = recv(dp.data) run dp.cont
  }

  sealed case class Out[A, B <: Session{type Dual = BDual}][BDual <: Session](data: A, cont: B) extends Session {
    // BDual =:= cont.Dual
    type Dual = In[A, BDual] // type inference provides second list

    def run(dp: Dual): Unit = cont run dp.recv(data)
  }

  def addServer =
    In{x: Int => 
    In{y: Int => System.out.println("Thinking")
    Out(x+y,
    Stop())}}

  def addClient =
    Out(3,
    Out(4, { System.out.println("Waiting")
    In{z: Int => System.out.println(z)
    Stop()}}))

  def myRun = addServer run addClient
}
