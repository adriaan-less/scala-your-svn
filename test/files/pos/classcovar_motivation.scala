// example by Paul Phillips
class ManifestWrapper[T: Manifest] {
 val clazz = manifest[T].erasure
 val interfaces = {
   def loop(current: Class[_]): List[Class[_]] = {
     if (current == null) Nil
     else current.getInterfaces.toList ++ loop(current.getSuperclass)
   }
   loop(clazz)
 }
 // This way: works
 //
 val interfaceMethods_works = Map(interfaces map (x => (x, x.getMethods.toList)) : _*)

 // This way:
 // <console>:15: error: could not find implicit value for parameter ev: <:<[(java.lang.Class[_$2], List[java.lang.reflect.Method]) forSome { type _$2 },(T, U)]
 //            val interfaceMethods = interfaces map (x => (x, x.getMethods.toList)) toMap
 //                                                                                  ^
 //
 val interfaceMethods = interfaces map (x => (x, x.getMethods.toList)) toMap
}