package scala.tools.nsc
package io

import java.io.{DataOutputStream, ByteArrayOutputStream}
import java.util.concurrent.{Executors, ExecutorService, LinkedBlockingQueue}

trait ConcurrentFileWriting {
  val global: Global
  import global.{settings, informProgress, error}

  private val cmdQ = new LinkedBlockingQueue[() => Unit]()
  private def drainCmdQ() = {
    import scala.collection.JavaConversions._
    val cmds = new java.util.LinkedList[() => Unit]()
    cmdQ drainTo cmds
    for(cmd <- cmds) cmd()
  }

  @inline implicit def pimpExecService(es: ExecutorService) = new { def exec(b: => Unit) = es.execute(new Runnable{
    def run() = try{
      b
    } catch {
      case t: Exception => if(settings.debug.value) t.printStackTrace() 
      cmdQ put {() => throw t}
    }})
  }
  private val writerThreadNb = Runtime.getRuntime().availableProcessors()
  private val ioThreadNb = writerThreadNb // should actually scale with number of IO channels
  private val classWriterPool = Executors.newFixedThreadPool(writerThreadNb)
  private val fileWriterPool = Executors.newFixedThreadPool(ioThreadNb)

  /** Performs `writer` on the output stream of `file`, and calls `informProgress` when that's done.
   *
   * Depending on the number of available processors, `writer` may be executed concurrently.
   * However, `informProgress` will run in the thread that calls `scheduleWrite` or `waitForWriters`.
   * Similarly, any exceptions caused while writing will be propagated to the calling thread.
   */
  def scheduleWrite(file: AbstractFile, msg: String = "wrote ")(writer: DataOutputStream => Unit) = 
    if(writerThreadNb > 1) {
      classWriterPool exec {
        val bytestream = new ByteArrayOutputStream()
        val outstream = new DataOutputStream(bytestream)
        writer(outstream)
        outstream.close()
        fileWriterPool exec {
          val filestream = file.output
          bytestream.writeTo(filestream)
          filestream.close()
          val progMsg: String = msg + file
          cmdQ put {() => informProgress(progMsg)}
        }
      }        
      drainCmdQ()
    } else { // non-concurrent
      val outstream = new DataOutputStream(file.output)
      writer(outstream)
      outstream.close()
      informProgress(msg + file)
    }

  /** Blocks until all writers are done.
   * writeClass must not be called subsequently.
   *
   * May perform pending informProgress's and throw exceptions that occurred concurrently.
   */
  def waitForWriters() = {
    def stop(pool: ExecutorService) = {
      pool.shutdown()
      if(!pool.awaitTermination(300, java.util.concurrent.TimeUnit.SECONDS))
        error("Writers did not finish in under 5 minutes after bytecode was generated!")
    } 
    stop(classWriterPool)
    stop(fileWriterPool)
    drainCmdQ()
  }
}