package hammer.test

import chisel3._
import chisel3.simulator.PeekPokeAPI
import chisel3.util._

object ClockingState extends Enumeration {
  val Continue, Done, Break, Ignore = Value
}

abstract class ClockingTask {

  private var actions: Seq[() => Unit] = Seq()

  /**
    * Delay some task after the rising edge of the clock
    *
    * @param action The action to be taken after the clock
    */
  def afterClock(action: () => Unit) = actions = actions.appended(action)

  private[hammer] def runAfterClock = {
    actions.map(_())
    actions = Seq()
  }

  def prestep(cycle: Int): Unit = {}
  def step(cycle:    Int): ClockingState.Value
}

class Clocking(clock: Clock, timeout: Int = 4096) extends PeekPokeAPI {
  var tasks = Seq[ClockingTask]()

  /**
    * Assign a new task for this `Clocking` instance
    * 
    * This is the simplified version that creates a STATELESS task. For full 
    * task creation, see `fork(task: ClockingTask)`
    * 
    * This function is named after the famous `fork` keyword of SystemVerilog, 
    * but the usage is slightly different
    * 
    * Each `task` of the `Clocking` is an instance that will be used in a loop 
    * per clock cycle. This process is something like:
    * 
    * @example {{{
    * 
    * while(true) {
    *   task1.execute();
    *   task2.execute();
    *   task3.execute();
    * 
    *   clock.step();
    * }
    * 
    * }}}
    * 
    * This function wraps the supplied `Int => ClockingState.Value` function into 
    * a `ClockingTask` instance.
    * 
    * `task`s are able to return a `ClockingState` that controls its execution 
    * behaviour. 
    * - `Continue`: this task will continue next loop
    * - `Done`: this task has finished, and will not be executed in the loop anymore
    * - `Break`: this task breaks the loop, causing the `Clocking` to stop immediately
    * - `Ignore`: this task will continue, but it will be counted as a `Done` task that allows simulation to end
    * 
    * A `Clocking` will stop running once all the tasks are done, or the timeout 
    * limit is reached
    * 
    * See `DecoupledTaskExt` in test/Extensions.scala for examples
    *
    * @param task a function that will run in the clocking loop
    * @return the `Clocking` instance for chaining
    */
  def fork(task: Int => ClockingState.Value): Clocking = fork(new ClockingTask {
    override def step(cycle: Int): ClockingState.Value = task(cycle)
  })

  /**
    * Assign a new task for this `Clocking` instance
    * 
    * This function is named after the famous `fork` keyword of SystemVerilog, 
    * but the usage is slightly different
    * 
    * Each `task` of the `Clocking` is an instance that will be used in a loop 
    * per clock cycle. This process is something like:
    * 
    * @example {{{
    * 
    * while(true) {
    *   task1.execute();
    *   task2.execute();
    *   task3.execute();
    * 
    *   clock.step();
    * }
    * 
    * }}}
    * 
    * `task`s are able to return a `ClockingState` that controls its execution 
    * behaviour. 
    * - `Continue`: this task will continue next loop
    * - `Done`: this task has finished, and will not be executed in the loop anymore
    * - `Break`: this task breaks the loop, causing the `Clocking` to stop immediately
    * 
    * A `Clocking` will stop running once all the tasks are done, or the timeout 
    * limit is reached
    * 
    * See `DecoupledTaskExt` in test/Extensions.scala for examples
    *
    * @param task a function that will run in the clocking loop
    * @return the `Clocking` instance for chaining
    */
  def fork(task: ClockingTask): Clocking = {
    tasks = tasks.appended(task)
    this
  }

  /**
    * Start the `Clocking` instance and run all the tasks
    * 
    * This function is blocking util all the tasks are stopped, or 
    */
  def run(): Unit = {
    val step = if (timeout < 0) 0 else 1

    clock.step()

    var cycle = 0
    for (_ <- 0.until(timeout, step)) {

      // Execute
      tasks.map(_.prestep(cycle))
      val results = tasks.map(t => (t, t.step(cycle)))

      // Clock
      clock.step()
      cycle += 1

      tasks.map(_.runAfterClock)

      // Break when any task returns ClockingState.Break
      if (results.map(_._2 == ClockingState.Break).reduce(_ || _)) {
        return
      }

      // Filter all the `Continue` & `Ignore` tasks
      tasks =
        results.filter(r => r._2 == ClockingState.Continue || r._2 == ClockingState.Ignore).map(
          _._1,
        )

      // Stops when tasks are all done (ignoring the `Ignore` tasks)
      if (results.filter(_._2 == ClockingState.Continue).length == 0) {
        return
      }
    }

    throw new RuntimeException(s"Clocking Timeout: exceeding $timeout cycles")
  }
}

object Clocking {

  /**
      * The Clocking API provides a single-threaded way to do paralleled tasks.
      * 
      * This function creates a `Clocking` instance that will automatically step the specific clock.  
      * If any task does not stop after `timeout` cycles, an exception will be thrown to fail the test.
      * 
      * @example {{{
      * Clocking(dut.clock, -1)
      *   // add a task
      *   .fork { cycle =>
      *     // this task will end at the 100th cycle
      *     return if(cycle == 100) ClockingState.Done else ClockingState.Continue
      *   }
      *   .run()    // only when `run` is called will the tasks be executed
      * }}}
      *
      * @param clock The clock to be stepped, usually the clock of the DUT
      * @param timeout The timeout cycle. A negative value will remove the timeout limit
      * @return A `Clocking` instance to be `fork`ed with tasks, then `run`
      */
  def apply(clock: Clock, timeout: Int = 4096) = new Clocking(clock, timeout)
}
