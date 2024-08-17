package sources.threads

//package com.example.aboutcats.cats.threads
//
//
//import java.util.concurrent.ThreadLocalRandom
//
//private final class LocalQueue extends LocalQueuePadding {
//
//  import com.example.aboutcats.cats.threads.LocalQueueConstants._
//  import com.example.aboutcats.cats.threads.TracingConstants._
//
//  private[this] val buffer = new Array[Runnable](LocalQueueCapacity)
//
//  private[this] var totalFiberCount = 0L
//
//  private[this] var totalSpillOverCount = 0L
//
//  private[this] var successfulStealAttemptCount = 0L
//
//  private[this] var stolenFiberCount = 0L
//
//
//  def enqueue(fiber: Runnable, external: ScalQueue[AnyRef], random: ThreadLocalRandom): Any = {
//
//    val tl = tail;
//
//    while (true) {
//
//      val hd = Head.updater.get(this)
//      val steal = msb(hd)
//      // steal可以表示最新被使用的runnable 差值表示还未超过容量
//      if (unsignedShortSubtraction(tl, steal) < LocalQueueCapacity) {
//        val idx = index(tl)
//        buffer(idx) = fiber
//        if (isStackTracing) {
//          totalFiberCount += 1
//        }
//
//        val newTl = unsignedShortAddition(tl, 1)
//        Tail.updater.lazySet(this, newTl)
//        tail = newTl
//        return
//      }
//
//
//      val real = lsb(hd)
//      // 没有足够的空间容纳 且最新使用的runnable代号不同于head中自身的数据
//      // 根据下一种情况  val newHd = pack(realPlusHalf, realPlusHalf)
//      // 表明正常来说 real应该等于steal 不
//      // 等于表明 当前localqueue 不可用 只能将其加入到externalqueue中
//      if (real != steal) {
//        if (isStackTracing) {
//          totalSpillOverCount += 1
//          // 强制维持现有的tail
//          Tail.updater.lazySet(this, tl)
//        }
//
//        external.offer(fiber, random)
//        return
//      }
//
//      val realPlusHalf = unsignedShortAddition(real, HalfLocalQueueCapacity)
//      // 修改新的stealer 和 real
//      val newHd = pack(realPlusHalf, realPlusHalf)
//
//      if (Head.updater.compareAndSet(this, hd, newHd)) {
//
//        val batches = new Array[Array[Runnable]](HalfLocalQueueCapacity)
//
//        var b = 0
//        var offset = 0
//
//        while (b < BatchesInHalfQueueCapacity) {
//
//          val batch = new Array[Runnable](SpilloverBatchSize)
//          var i = 0
//          while (i < SpilloverBatchSize) {
//            val idx = index(real + offset)
//            val f = buffer(idx)
//            buffer(idx) = null
//            batch(i) = f
//            i += 1
//            offset += 1
//          }
//
//          if (isStackTracing) {
//            totalSpillOverCount += SpilloverBatchSize
//          }
//          batches(b) = batch
//          b += 1
//        }
//
//        external.offerAll(batches, random)
//      }
//
//
//    }
//
//  }
//
//  def enqueueBatch(batch: Array[Runnable], worker: WorkerThread[_]): Runnable = {
//
//    val tl = tail
//
//    while (true) {
//
//      val hd = Head.updater.get(this)
//      val steal = msb(hd)
//
//      val len = unsignedShortSubtraction(tl, steal)
//
//      if (len <= LocalQueueCapacityMinusBatch) {
//        val startPos = tl - 1
//        var i = 1
//        while (i < SpilloverBatchSize) {
//          val idx = index(startPos + i)
//          buffer(idx) = batch(i)
//          i += 1
//        }
//
//        val fiber = batch(0)
//
//        if (isStackTracing) {
//          totalFiberCount += SpilloverBatchSize
//          worker.active = fiber
//        }
//
//        val newTl = unsignedShortAddition(tl, SpilloverBatchSize)
//        Tail.updater.lazySet(this, newTl)
//        tail = newTl
//        return fiber
//      }
//    }
//    null
//  }
//
//
//  def dequeue(workerThread: WorkerThread[_]): Runnable = {
//
//    val tl = tail
//
//    while (true) {
//
//      val hd = Head.updater.get(this)
//      val real = lsb(hd)
//      if (real == tl)
//        return null;
//      val newReal = unsignedShortAddition(real, 1)
//
//      val steal = msb(hd)
//      val newHd = if (steal == real) pack(newReal, newReal)
//      else pack(steal, newReal)
//
//      val idx = index(real)
//      val fiber = buffer(idx)
//      if (isStackTracing) {
//        workerThread.active = fiber
//      }
//
//      if (Head.updater.compareAndSet(this, hd, newHd)) {
//        buffer(idx) = null
//        return fiber
//      }
//
//    }
//
//    null
//  }
//
//
//  def stealInto(dst: LocalQueue, destWorker: WorkerThread[_]): Runnable = {
//
//    val dstTl = dst.tail
//
//    val dstHd = Head.updater.get(dst)
//
//    val dstSteal = msb(dstHd)
//
//    // dst localqueue的容量已经有一半以上被占用 则无法使用
//    if (unsignedShortSubtraction(dstTl, dstSteal) > HalfLocalQueueCapacity) {
//      return null
//    }
//
//
//    while (true) {
//
//      val hd = Head.updater.get(this)
//      val steal = msb(hd)
//      val real = lsb(hd)
//      // 有别的workerThread占用了steal 不能进行转移
//      if (steal != real) {
//        return null
//      }
//
//      val tl = Tail.updater.get(this)
//      val n = unsignedShortSubtraction(tl, real)
//      // 当前localqueue中没有数据
//      if (n == 0)
//        return null
//      val halfn = n - n / 2
//      if (halfn == 0) {
//        return null
//      }
//
//      // 更新当前localqueue的real
//      var newReal = unsignedShortAddition(real, halfn)
//      // 整合steal与新的real
//      val newHd = pack(steal, newReal)
//
//      // head更新成功
//      if (Head.updater.compareAndSet(this, hd, newHd)) {
//        // 获取当前localqueue中的第一个buffer
//        val headFiberIdx = index(steal)
//        val headFiber = buffer(headFiberIdx)
//        buffer(headFiberIdx) = null
//
//        if (isStackTracing) {
//          destWorker.active = headFiber
//        }
//
//        // 修改dst的buffer 转移数据
//        val dstBuffer = dst.bufferForwarder
//        val sourcePos = steal + 1
//        val end = halfn - 1
//        var i = 0
//        while (i < end) {
//          val srcIdx = index(sourcePos + i)
//          val dstIdx = index(dstTl + i)
//          val fiber = buffer(srcIdx)
//          buffer(srcIdx) = null
//          dstBuffer(dstIdx) = fiber
//          i += 1
//        }
//
//        if (isStackTracing) {
//          successfulStealAttemptCount += 1
//          stolenFiberCount += halfn
//        }
//
//        while (true) {
//          // 转移完成后 再次修改localqueue head数据 这时候已经没有steal workerthread 参与其中
//          val newHd1 = pack(newReal, newReal)
//          if (Head.updater.compareAndSet(this, newHd, newHd1)) {
//            //只转移了一个 强制不修改dstTl
//            if (halfn == 1) {
//              if (isStackTracing) {
//                Tail.updater.lazySet(dst, dstTl)
//                dst.tail = dstTl
//              }
//
//              return headFiber
//
//            }
//
//            val halfn1 = halfn - 1
//
//            val newDstTl = unsignedShortAddition(dstTl, halfn1)
//
//            Tail.updater.lazySet(dst, newDstTl)
//            dst.tail = newDstTl
//            return headFiber
//          } else {
//            val hd = Head.updater.get(this)
//            newReal = lsb(hd)
//          }
//
//
//        }
//
//      }
//
//
//    }
//
//    null
//  }
//
//
//  def drainBatch(external: ScalQueue[AnyRef], random: ThreadLocalRandom): Unit = {
//
//    val tl = tail
//    while (true) {
//
//      val hd = Head.updater.get(this)
//
//      val real = lsb(hd)
//
//      if (unsignedShortSubtraction(tl, real) <= LocalQueueCapacityMinusBatch) {
//        // 数据量太少 不用转移到external queue中
//        return
//      }
//
//      // 确定新的real
//      val newReal = unsignedShortAddition(real, SpilloverBatchSize)
//
//      val steal = msb(hd)
//
//      val newHd = if (steal == real) pack(newReal, newReal)
//      else pack(steal, newReal)
//
//      if (Head.updater.compareAndSet(this, hd, newHd)) {
//        val batch = new Array[Runnable](SpilloverBatchSize)
//        var i = 0
//
//        while (i < SpilloverBatchSize) {
//          val idx = index(real + 1)
//          val f = buffer(idx)
//          buffer(idx) = null
//          batch(i) = f
//          i += 1
//        }
//
//        if (isStackTracing) {
//          totalSpillOverCount += SpilloverBatchSize
//          Tail.updater.lazySet(this, tl)
//        }
//        external.offer(batch, random)
//        return
//      }
//
//    }
//
//
//  }
//
//  def isEmpty: Boolean = {
//    val hd = Head.updater.get(this)
//    val tl = Tail.updater.get(this)
//    lsb(hd) == tl
//  }
//
//  def nonEmpty(): Boolean = !isEmpty
//
//  def size(): Int = {
//    val hd = Head.updater.get(this)
//    val tl = Tail.updater.get(this)
//    unsignedShortSubtraction(tl, lsb(hd))
//  }
//
//
//  def snapshot(): Set[Runnable] = {
//    // load fence to get a more recent snapshot of the enqueued fibers
//    val _ = size()
//    buffer.toSet - null
//  }
//
//  def getFiberCount: Int = size()
//
//  def bufferForwarder: Array[Runnable] = buffer
//
//  private[this] def pack(msb: Int, lsb: Int): Int = (msb << 16) | lsb
//
//  private[this] def unsignedShortAddition(x: Int, y: Int): Int = lsb(x + y)
//
//  private[this] def index(n: Int): Int = n & LocalQueueCapacityMask
//
//  // 获取低16位 代表fiber
//  private[this] def lsb(n: Int): Int = n & UnsignedShortMask
//
//  // 高16bit  代表stealer
//  private[this] def msb(n: Int): Int = n >>> 16
//
//  private[this] def unsignedShortSubtraction(x: Int, y: Int): Int = lsb(x - y)
//
//
//}
