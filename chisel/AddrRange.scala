package hammer

import chisel3._
import chisel3.util._

case class AddrRange(start: BigInt, length: BigInt) {
  def end                  = start + length
  def isInside(addr: UInt) = addr >= start.U && addr < end.U

  override def toString() = s"[$start, ${start + length})"
}

object AddrRange {

  def getFirstOverlap(addrs: Seq[AddrRange]): Option[(AddrRange, AddrRange)] = {
    // We first sort the address ranges by starting address, then compare them one by one
    val sorted = addrs.sortBy(_.start)
    for (i <- 0 until (sorted.length - 1)) {
      if (sorted(i).end > sorted(i + 1).start) {
        return Some(sorted(i), sorted(i + 1))
      }
    }
    return None
  }

  def isOverlap(addrs: Seq[AddrRange]): Boolean = getFirstOverlap(addrs).isDefined
}
