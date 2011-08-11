object Test extends App {
 4 match { 
   case 1 | 2  => println("FAILED") 
   case x@(4 | 5 | 6) => println("OK "+ x) 
   case 7  => println("FAILED") 
 }
}