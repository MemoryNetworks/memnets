package memnets.fx.utils

import javafx.concurrent.Worker
import memnets.model._
import scalafx.beans.property._

object TaskP {
  def apply[T](msg: String)(op: => T): TaskP[T] = {
    new TaskP[T] {
      updateMessage(msg) // update b4 call or might not see message
      def call(): T = {
        op
      }
    }
  }
}
abstract class TaskP[T] extends javafx.concurrent.Task[T] {
  def updateProgressP(workDone: Double, max: Double) = updateProgress(workDone, max)
  def updateMessageP(msg: String): Unit = updateMessage(msg)
  def isReady = stateProperty.getValue == Worker.State.READY
}

object JTaskSupport {
  implicit class TaskExt[T](val task: javafx.concurrent.Task[T]) extends AnyVal {
    def start()(implicit tr: JTaskSupport): Unit = tr.startTask(task)
  }

}

/** only supports one background task.  no support for cancel, which requires task.callable to poll cancel   */
abstract class JTaskSupport extends Logging {
  protected val _currentTask = ObjectProperty[Option[javafx.concurrent.Task[_]]](initialValue = None)
  val taskRunning = currentTask =!= None

  /** does not start thread.  just checks running and updates currentTask */
  def startTask[T](task: javafx.concurrent.Task[T]): Unit = {
    if (taskRunning.get)
      multiTaskError()

    _currentTask.value = Option(task)

    startTaskHelper(task)

    new Thread(task) { setDaemon(true) }.start()
  }
  def currentTask: ReadOnlyObjectProperty[Option[javafx.concurrent.Task[_]]] = _currentTask
  def multiTaskError(): Unit
  def progress(msg: String): Unit = {
    for (taskP <- taskPOption) {
      logger.debug("progress: " + msg + " for task: " + taskP)
      taskP.updateMessageP(msg)
    }
  }
  def progress(workDone: Double, max: Double = 1.0): Unit = {
    if (workDone > 0.0) {
      for (taskP <- taskPOption)
        taskP.updateProgressP(workDone, max)
    }
  }
  protected def taskPOption: Option[TaskP[_]] = currentTask.value.flatMap(_.as[TaskP[_]])
  protected def startTaskHelper[T](task: javafx.concurrent.Task[T]): Unit = {}
}
