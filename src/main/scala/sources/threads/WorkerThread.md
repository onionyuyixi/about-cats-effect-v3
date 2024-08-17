
 A counter (modulo `ExternalQueueTicks`) which represents the
 `WorkerThread` finite state machine. The following values have special
 semantics explained here:

   0: To increase the fairness towards fibers scheduled by threads which
      are external to the `WorkStealingThreadPool`, every
      `ExternalQueueTicks` number of iterations, **the external queue takes
      precedence over the local queue.**

      If a fiber is successfully dequeued from the external queue, it will
      be executed immediately. If a batch of fibers is dequeued instead,
      the whole batch is enqueued on the local queue and other worker
      threads are notified of existing work available for stealing. The
      `WorkerThread` unconditionally transitions to executing fibers from
      the local queue (state value 4 and larger).

      This state occurs "naturally" after a certain number of executions
      from the local queue (when the state value wraps around modulo
      `ExternalQueueTicks`).

   1: Fall back to checking the external queue after a failed dequeue from
      the local queue. Depending on the outcome of this check, the
      `WorkerThread` transitions to executing fibers from the local queue
      in the case of a successful dequeue from the external queue (state
      value 4 and larger). Otherwise, the `WorkerThread` continues with
      asking for permission to steal from other `WorkerThread`s.

      Depending on the outcome of this request, the `WorkerThread` starts
      looking for fibers to steal from the local queues of other worker
      threads (if permission was granted, state value 2), or parks
      directly. In this case, there is less bookkeeping to be done
      compared to the case where a worker was searching for work prior to
      parking. After the worker thread has been unparked, it transitions
      to looking for work in the external queue (state value 3) while also
      holding a permission to steal fibers from other worker threads.

   2: The `WorkerThread` has been allowed to steal fibers from other
      worker threads. If the attempt is successful, the first fiber is
      executed directly and the `WorkerThread` transitions to executing
      fibers from the local queue (state value 4 and larger). If the
      attempt is unsuccessful, the worker thread announces to the pool
      that it was unable to find any work and parks.

   3: The `WorkerThread` has been unparked an is looking for work in the
      external queue. If it manages to find work there, it announces to
      the work stealing thread pool that it is no longer searching for
      work and continues to execute fibers from the local queue (state
      value 4 and larger). Otherwise, it transitions to searching for work
      to steal from the local queues of other worker threads because the
      permission to steal is implicitly held by threads that have been
      unparked (state value 2).

   4 and larger: Look for fibers to execute in the local queue. In case
      of a successful dequeue from the local queue, increment the state
      value. In case of a failed dequeue from the local queue, transition
      to looking for fibers in the external queue (state value 1).

 A note on the implementation. Some of the states seem like they have
 overlapping logic. This is indeed true, but it is a conscious decision.
 The logic is carefully unrolled and compiled into a shallow `tableswitch`
 instead of a deeply nested sequence of `if/else` statements. This change
 has lead to a non-negligible 15-20% increase in single-threaded
 performance.
/