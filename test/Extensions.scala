package hammer

import chisel3._
import chisel3.simulator.PeekPokeAPI
import chisel3.util._

package object test {
  implicit class DecoupledExt[T <: Data](self: DecoupledIO[T])
      extends PeekPokeAPI {

    /**
      * Peek to see if this `DecoupledIO` is firing
      *
      * @return whether this IO is firing data
      */
    def peekFire() = self.valid.peekBoolean() && self.ready.peekBoolean()

   /**
     * Send a data via DecoupledIO
     *
     * @param data The data to be sent
     * @param clock The device clock
     * @param timeout The clock cycle timeout
     */ 
    def send(data: T, clock: Clock, timeout: Int): Unit = {
      self.valid.poke(true)
      self.bits.poke(data)

      var i = 0
      while (!self.peekFire()) {
        clock.step()
        i += 1
        if (i >= timeout)
          throw new RuntimeException(
            s"DecoupledIO ${self} takes more than $timeout cycles to send, reaching timeout",
          )
      }
      clock.step()
      self.valid.poke(false)
    }

    /**
      * Receive a data via Decoupled IO and operate on it
      *
      * @param op The operation to be applied
      * @param clock The device clock
      * @param timeout The clock cycle timeout
      */
    def recv(op: T => Unit, clock: Clock, timeout: Int) = {
      self.ready.poke(true)

      var i = 0
      while (!self.peekFire()) {
        clock.step()
        i += 1
        if (i >= timeout)
          throw new RuntimeException(
            s"DecoupledIO ${self} takes more than $timeout cycles to wait for response, reaching timeout",
          )
      }
      clock.step()

      op(self.bits)
      self.ready.poke(false)
    }

    /**
      * Receive a data via Decoupled IO
      *
      * @param clock The device clock
      * @param timeout The clock cycle timeout
      * @return The received data
      */
    def recv(clock: Clock, timeout: Int): T = {
      var result: T = null.asInstanceOf[T]
      recv(t => result = t, clock, timeout)
      result
    }

    /**
      * Receive a data and expect its value
      *
      * @param expected The expected value
      * @param clock The device clock
      * @param timeout The clock cycle timeout
      */
    def recvExpect(expected: T, clock: Clock, timeout: Int): Unit =
      recv(t => t.expect(expected), clock, timeout)
  }

  implicit class DecoupledTaskExt[T <: Data](self: DecoupledIO[T])
      extends PeekPokeAPI {

    /**
      * Create a data sending task
      *
      * @param data The data to be sent
      * @return The task, can be directly used with `Clocking`
      */
    def send(data: T): ClockingTask = new ClockingTask {
      override def executePreClock(cycle: Int): Unit = {
        self.valid.poke(true)
        self.bits.poke(data)
      }

      def executePostClock(cycle: Int): ClockingState.Value =
        if (self.peekFire()) {
          self.valid.poke(false)
          ClockingState.Done
        } else ClockingState.Continue
    }

    /**
      * Create a data sending task that sends a Seq of data sequencially
      *
      * @param data the data to be sent
      * @return the `ClockingTask`
      */
    def send(data: Seq[T]): ClockingTask = new ClockingTask {
      var queue = data

      override def executePreClock(cycle: Int): Unit = {
        self.valid.poke(true)
        self.bits.poke(queue.head)
      }

      def executePostClock(cycle: Int): ClockingState.Value =
        if (self.peekFire()) {
          self.valid.poke(false)
          queue = queue.drop(1)

          if (queue.length == 0) ClockingState.Done
          else ClockingState.Continue

        } else ClockingState.Continue
    }
  }
}
