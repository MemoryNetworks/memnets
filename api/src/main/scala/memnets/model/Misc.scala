package memnets.model

trait ModelLayout { def layout(): Unit }
trait TickListener { def tick(te: Tick): Unit }
trait Tickable extends TickListener {
  def init(): Unit = { reset() } // called when first added and if changes
  def reset(): Unit = {}
  def destroy(): Unit = {}
}
trait TickFunction { def eval(te: Tick): Double }
