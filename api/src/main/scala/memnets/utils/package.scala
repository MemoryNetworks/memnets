package memnets

import java.io._
import java.net.URL

import com.typesafe.scalalogging.StrictLogging

import scala.io._
import scala.reflect.ClassTag
import scala.util.Random

package object utils extends StrictLogging {
  implicit class StringExt(val f: String) extends AnyVal {
    def toFile = new File(f)
    def changeFileExt(ext: String): String = { f.substring(0, f.indexOf(".")) + "." + ext }
    def relativeFile: String = if (f.startsWith("/")) f else "/" + f
    def asURL: URL = {
      //    val resource = getClass.getResource(File.separator + file)
      val url = getClass.getResource(relativeFile)
      if (url == null) throw new IOException(s"Cannot load resource: $f")
      url
    }
    def asStream: InputStream = {
      val is = getClass.getResourceAsStream(relativeFile)
      if (is == null) throw new IOException(s"Cannot load input stream: $f")
      is
    }
    def streamToTempFile(name: String = "tempfile"): File = {
      val text = readStream()
      val file = File.createTempFile(name, ".tmp")
      file.printTo(_.println(text))
      file
    }
    def readStream(): String = {
      val buff = Source.fromInputStream(relativeFile.asStream)
      val text = buff.mkString
      buff.close()
      text
    }
    def processStream(op: (Int, String) => Any): Unit = {
      var buff: BufferedSource = null.asInstanceOf[BufferedSource]
      try {
        buff = Source.fromInputStream(relativeFile.asStream)
        for ((line, i) <- buff.getLines.zipWithIndex)
          op(i, line)
      } catch {
        case th: Throwable =>
          logger.error("file : " + f, th)
          throw th
      } finally {
        if (buff != null)
          buff.close()
      }
    }
  }
  implicit def stringToFile(s: String) = new File(s)
  implicit class FileExt(val f: File) extends AnyVal {
    def read(): Option[String] = {
      try {
        val source = scala.io.Source.fromFile(f)
        val lines = source.mkString
        source.close
        Some(lines)
      } catch {
        case t: Throwable =>
          logger.error(s"could not read file: ${f.getCanonicalPath}", t)
          None
      }
    }
    def printTo(op: java.io.PrintWriter => Unit): Unit = {
      require(op != null)
      val p = new java.io.PrintWriter(f)
      try {
        op(p)
      } finally {
        p.close()
      }
    }
  }

  implicit class ArrayExt[T](val array: Array[T]) extends AnyVal {
    def randomPick: T = array(Random.nextInt(array.length))
    def randomPicks(n: Int = 5): IndexedSeq[T] = for (i <- 0 until n) yield randomPick
    def shuffle(): Unit = {
      val random = new Random()
      val count = array.length
      var i = count
      while (i > 1) {
        swap(array, i - 1, random.nextInt(i))
        i -= 1
      }
    }
    private def swap(array: Array[T], i: Int, j: Int): Unit = {
      val temp = array(i)
      array(i) = array(j)
      array(j) = temp
    }
    def grow(factor: Double = 2.0)(implicit tag: ClassTag[T]): Array[T] = {
      val newArray = Array.ofDim[T]((array.length * factor).toInt)
      array.copyToArray(newArray)
      newArray
    }
    def prettyPrint(take: Int = 16): String = {
      array.iterator.take(take).mkString("{", ",", "]")
    }
    def arrayCopy(dest: Array[T]): Unit = System.arraycopy(array, 0, dest, 0, dest.length)
  }
  object para {
    import scala.concurrent._
    import scala.concurrent.duration._
    //  import scala.concurrent.ExecutionContext.Implicits.global  // don't make method local!!!
    //implicit val ec = scala.concurrent.ExecutionContext.Implicits.global  // don't make method local!!!
    val numCores = Runtime.getRuntime.availableProcessors
    logger.debug(s"numCores: $numCores")
    val pool = new java.util.concurrent.ForkJoinPool(numCores)
    implicit val ec = ExecutionContext.fromExecutorService(pool)

    implicit class FutureIterableExt[R](val iter: Iterable[Future[R]]) extends AnyVal {
      def paraWait: Unit = for (fu <- iter) Await.ready(fu, Duration.Inf)
      def reduceWait: Iterable[R] = for (fu <- iter) yield Await.result(fu, Duration.Inf)
    }
    final def paraWait(fs: Iterable[Future[_]]): Unit = for (fu <- fs) Await.ready(fu, Duration.Inf)
    implicit class IndexedSeqParaExt[T](val seq: IndexedSeq[T]) extends AnyVal {
      def paraF(f: (Int, Int, IndexedSeq[T]) => Unit): Unit = {
        val g: (Int, Int) => Unit = f(_, _, seq)
        para(0, seq.length, 3)(g)
      }
    }
    final def para(start: Int, end: Int, threshold: Int = 80000)(f: (Int, Int) => Unit): Unit = {
      val size = end - start
      if (size >= threshold) {
        val div = size / 4
        val one = start + div
        val mid = one + div
        val three = mid + div
        List(
          Future { f(start, one) },
          Future { f(one, mid) },
          Future { f(mid, three) },
          Future { f(three, end) }
        ).paraWait
      } else
        f(start, end)
    }

    final def reduce(start: Int, end: Int, threshold: Int = 80000)(f: (Int, Int) => Double): Double = {
      val size = end - start
      if (size > threshold) {
        val div = size / 4
        val one = start + div
        val mid = one + div
        val three = mid + div
        List(
          Future { f(start, one) },
          Future { f(one, mid) },
          Future { f(mid, three) },
          Future { f(three, end) }
        ).reduceWait.sum
      } else
        f(start, end)
    }
  }
}
