trait coll[m[x <: y], y]

class FooString[x <: String]

object ok extends coll[FooString, String]