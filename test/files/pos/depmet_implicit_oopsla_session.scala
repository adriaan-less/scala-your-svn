object Sessions {
  trait Session[A] {
    type Dual

    def run(p: A, dp: Dual): Unit
  }

  implicit object StopSession extends Session[Stop] {
    type Dual = Stop

    def run(p: Stop, dp: Stop): Unit = {}
  }

  implicit def InDual[A, B](implicit sessionD: Session[B]) = 
    new Session[In[A, B]] {
      type Dual = Out[A, sessionD.Dual]

      def run(p: In[A, B], dp: Dual): Unit = 
        sessionD.run(p.func(dp.x), dp.y)
  }

  implicit def OutDual[A, B](implicit sessionD: Session[B]) = 
    new Session[Out[A, B]] {
     type Dual = In[A, sessionD.Dual]

     def run(p: Out[A, B], dp: In[A, sessionD.Dual]): Unit = 
       sessionD.run(p.y, dp.func(p.x))
  }

  sealed case class Stop
  sealed case class In[-A, +B](func: A => B)
  sealed case class Out[+A, +B](x: A, y: B)

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

  def runSession[A, D1](p: A, dp: D1)(implicit s: Session[A]{type Dual=D1}) =
    s.run(p, dp)

  def myRun = runSession(addServer, addClient)
}