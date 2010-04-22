/*-enable-unique*/
import scala.annotation.{unique, transient}
import scala.annotation.Unique.capture

import java.io.File

@transient class TFile(name: String) extends File(name)

class C {

  def doWithX[X](fileName: String, func: (TFile => X)@unique): X = {
    val f = new TFile(fileName)
    func(f)
  }

  var leaky: File = _

  doWithX("test.xml", { f =>
    println(f.getCanonicalPath)
    // error: capability of right-hand side is not shared
    leaky = capture(f, leaky)
  })

}
