/*-enable-unique*/
import scala.annotation.unique

import java.io.File

@transient class TFile(name: String) extends File(name)

class C {

  def doWithX[X](fileName: String, func: (TFile => X)@unique): X = {
    val f = new TFile(fileName)
    func(f)
  }

  doWithX("test.xml", { f =>
    println(f.getCanonicalPath)
  })

}
