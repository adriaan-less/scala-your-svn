package scala.concurrent;

import scala.collection.mutable.LinkedList;

class Channel[a] with Monitor {
  var dummy: a = _;
  private var written = new LinkedList[a](dummy, null);
  private var lastWritten = written;
  private var nreaders = 0;

  def write(x: a) = synchronized {
    lastWritten.next = new LinkedList(x, null);
    lastWritten = lastWritten.next;
    if (nreaders > 0) notify();
  }

  def read: a = synchronized {
    if (written.next == null) {
      nreaders = nreaders + 1; wait(); nreaders = nreaders - 1;
    }
    written = written.next;
    written.elem;
  }
}
