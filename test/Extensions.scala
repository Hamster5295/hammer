package hammer

import chisel3._
import chisel3.simulator.PeekPokeAPI
import chisel3.util._

package object test {
  implicit class DecoupledExt[T <: Data](self: DecoupledIO[T]) extends PeekPokeAPI {

    def peekFire() = self.valid.peekBoolean() && self.ready.peekBoolean()

    def send(data: T, clock: Clock, timeout: Int = 256): Unit = {
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

    def recvOp(op: T => Unit, clock: Clock, timeout: Int = 256) = {
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

    def recv(clock: Clock, timeout: Int = 256): T = {
      var result: T = null.asInstanceOf[T]
      recvOp(t => result = t, clock, timeout)
      result
    }

    def recvExpect(expected: T, clock: Clock, timeout: Int = 256): Unit =
      recvOp(t => t.expect(expected), clock, timeout)
  }
}
