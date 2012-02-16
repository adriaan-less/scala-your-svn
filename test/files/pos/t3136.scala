class Type
class Symbol
case class PolyType(tps: List[Symbol], res: Type) extends Type
class OtherType extends Type

// case class NullaryMethodType(tp: Type) extends Type

object NullaryMethodType {
  def apply(resTpe: Type): Type = PolyType(List(), resTpe)
  def unapply(tp: Type): Option[(Type)] = None
}

object Test {
  def TEST(tp: Type): String = 
    tp match {
      case PolyType(ps1, PolyType(ps2, res @ PolyType(a, b))) => "1"+tp // couldn't find a simpler version that still crashes
      case NullaryMethodType(meh) => "2"+meh
    }
}

/* error:

[   insert2]  Row(0)((a @ _, b @ _, ps2 @ _, ps1 @ _))
[   insert2]  Row(1)((_, _, _, _))
...
Creating index 0: mtype = (val ps1: List[Symbol],val ps2: List[Symbol],val res: PolyType,val a: List[Symbol],val b: Type)java.lang.String
[New label] def body%0(value ps1, value ps2, value res, value a, value b): java.lang.String = "1".+(tp)

....
Creating index 1: mtype = (val meh: Type)java.lang.String // XXX shouldn't this be the subpattern?
[New label] def body%1(value meh): java.lang.String = "2".+(meh)
[    toTree]  labeldef body%1(meh) = "2".+(meh)
[    toTree]  if (temp13.isInstanceOf[PolyType]){
  var temp14: PolyType = temp13.asInstanceOf[PolyType];
  val temp15: List[Symbol] = temp14.tps;
  var temp16: Type = temp14.res;
  val ps1: List[Symbol] = temp9;
  val ps2: List[Symbol] = temp12;
  val a: List[Symbol] = temp15;
  val b: Type = temp16;
  labeldef body%0(ps1,ps2,res,a,b) = "1".+(tp)
}else
  labeldef body%1(meh) = "2".+(meh)

Exception in thread "main" scala.tools.nsc.symtab.Types$TypeError: not enough arguments for method body%1: (val meh: Type)java.lang.String.
Unspecified value parameter meh.


*/
