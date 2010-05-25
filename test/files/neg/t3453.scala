class A
class B

trait S {
    implicit def aToB(a: A): B = new B
}

class T extends S {
    def x: B = {
        val aToB = 3
        // doesn't compile, because aToB method requires 'T.this.' prefix
        //aToB(new A)

        new A
        // compiles, using T.this.aToB,
        //   despite it not being accessible without a prefix
    }
}