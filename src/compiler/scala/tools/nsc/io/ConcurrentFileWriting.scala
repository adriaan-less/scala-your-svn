package scala.tools.nsc
package io

import java.io.DataOutputStream
import java.util.concurrent.{Executors, ExecutorService, LinkedBlockingQueue}

trait ConcurrentFileWriting {
  val global: Global
  import global.{settings, informProgress, error}

  /** must be called before scheduleWrite and waitForWriters in order to allow concurrent writes*/
  def setupWriters() = {
    nThreads = Runtime.getRuntime().availableProcessors()
    writerPool = Executors.newFixedThreadPool(nThreads)
    cmdQ = new LinkedBlockingQueue[() => Unit]()
  }

  /** Performs `writer` on the output stream of `file`, and calls `informProgress` when that's done.
   *
   * Depending on the number of available processors, `writer` may be executed concurrently.
   * However, `informProgress` will run in the thread that calls `scheduleWrite` or `waitForWriters`.
   * Similarly, an exception that was caused while writing will be propagated to the calling thread.
   */
  def scheduleWrite(file: AbstractFile, msg: String = "wrote ")(writer: DataOutputStream => Unit) = 
    if(nThreads > 1) {
      writerPool.execute(new Writer(file, writer, msg))
      drainCmdQ()
    } else { // non-concurrent
      val outstream = new DataOutputStream(file.output)
      writer(outstream)
      outstream.close()
      informProgress(msg + file)
    }

  /** Blocks until all writers are done.
   * 
   * May perform pending informProgress's and throw the first of the exceptions that occurred concurrently.
   */
  def shutdownWriters() = if(writerPool ne null) {
    writerPool.shutdown()
    if(!writerPool.awaitTermination(300, java.util.concurrent.TimeUnit.SECONDS))
      error("Writers did not finish in under 5 minutes after bytecode was generated!")
    drainCmdQ()
    nThreads = 0; writerPool = null; cmdQ = null
  }

  // must only be set by setupWriters and shutdownWriters:
  private var nThreads = 0
  private var writerPool: ExecutorService = null
  private var cmdQ: LinkedBlockingQueue[() => Unit] = null

  private class Writer(val file: AbstractFile, val writer: DataOutputStream => Unit, val msg: String) extends Runnable {
    def run(): Unit = {
      try {
        val outstream = new DataOutputStream(file.bufferedOutput)
        writer(outstream)
        outstream.close()
        val progMsg: String = msg + file
        cmdQ put {() => informProgress(progMsg)}
      } catch {
        case ex: Exception => 
          if(settings.debug.value) ex.printStackTrace()
          cmdQ put {() => throw ex}
          throw ex
      }
    }
  }
  
  // executes commands that arise in worker threads in main thread (informProgress/throwing an exception)
  // TODO: when multiple exceptions re-throws are put in the queue, the first one will cause drainCmdQ to abort
  //       should we aggresgate all exceptions into a single one instead?
  private def drainCmdQ() = {
    import scala.collection.JavaConversions._
    val cmds = new java.util.LinkedList[() => Unit]()
    cmdQ drainTo cmds
    for(cmd <- cmds) cmd()
  }
}