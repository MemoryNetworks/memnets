package memnets.core

import memnets.model.Logging

trait SimStepper {
  def sim: Sim
  def next(playing: Boolean): Unit
  def reset(): Unit
  def step(count: Int): Unit
}

final class SingleThreadStepper(val sim: Sim) extends SimStepper {
  def next(playing: Boolean): Unit = {}
  def reset(): Unit = {}
  def step(count: Int): Unit = {
    var i = 0
    while (i < count) {
      sim.step()
      i += 1
    }
  }
}

final class FutureStepper(val sim: Sim) extends SimStepper with Logging {
  logger.debug("future stepper created")
  import concurrent._
  import concurrent.duration._

  private implicit val localEC = memnets.utils.para.ec
  private val timeout = 12 seconds
  private var tickTask: Future[Unit] = Future {}
  private var _lastCount = 1
  def next(playing: Boolean): Unit = {
    tickTask =
      if (playing)
        Future {
          var i = 0
          while (i < _lastCount) {
            sim.step()
            i += 1
          }
        } else
        Future {}
  }
  def reset(): Unit = {
    Await.result(tickTask, timeout)
    tickTask = Future {}
  }
  def step(count: Int): Unit = {
    Await.result(tickTask, timeout)
    _lastCount = count
  }
}
