package scala.tools.nsc
package io

import java.io.DataOutputStream
import java.util.concurrent.{Executors, LinkedBlockingQueue}

trait ConcurrentFileWriting {
  val global: Global
  import global.{settings, informProgress}

  private val cmdQ = new LinkedBlockingQueue[() => Unit]()
  private def drainCmdQ() = {
    import scala.collection.JavaConversions._
    val cmds = new java.util.LinkedList[() => Unit]()
    cmdQ drainTo cmds
    for(cmd <- cmds) cmd()
  }

  private val nThreads = Runtime.getRuntime().availableProcessors()
  private val writerPool = Executors.newFixedThreadPool(nThreads)

  private class Writer(val file: AbstractFile, val writer: DataOutputStream => Unit, val msg: String) extends Runnable {
    def run(): Unit = {
      try {
        val outstream = new DataOutputStream(file.bufferedOutput)
        writer(outstream)
        outstream.close()
        val progMsg: String = msg + file
        cmdQ put {() => informProgress(progMsg)}
      } catch {
        case t: Exception => 
          if(settings.debug.value) t.printStackTrace()
          cmdQ put {() => throw t}
      }
    }
  }

  /** Performs `writer` on the output stream of `file`, and calls `informProgress` when that's done.
   *
   * Depending on the number of available processors, `writer` may be executed concurrently.
   * However, `informProgress` will run in the thread that calls `scheduleWrite` or `waitForWriters`.
   * Similarly, any exceptions caused while writing will be propagated to the calling thread.
   */
  def scheduleWrite(file: AbstractFile, msg: String = "wrote ")(writer: DataOutputStream => Unit) = 
    if(nThreads > 1) {
      writerPool.execute(new Writer(file, writer, msg))
      drainCmdQ()
    }
    else { // non-concurrent
      val outstream = new DataOutputStream(file.bufferedOutput)
      writer(outstream)
      outstream.close()
      informProgress(msg + file)
    }

  /** Blocks until all writers are done.
   * 
   * May perform pending informProgress's and throw exceptions that occurred concurrently.
   */
  def waitForWriters() = {
    writerPool.shutdown()
    drainCmdQ()
  }
}