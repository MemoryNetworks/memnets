package memnets

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.Gaussian
import com.typesafe.scalalogging.StrictLogging
import memnets.core.{GpuSync, OdeMethod}
import memnets.linalg._
import memnets.model.impl._

import scala.reflect.Manifest
import scala.util.Random

package object model extends StrictLogging {
  type Logging = com.typesafe.scalalogging.StrictLogging
  type TieType = Param
  type E = linalg.W // non-generic type here makes life much easier in Drools/Java
  type OP = Double => Double
  type Tgt = AbstractLayer
  type SpikeListener = Int => Unit
  type LayerInit = Int => DenseVector[Double]
  type Grd = GridData
  case class WorkingDir(dir: String)

  implicit def cfToGradHints(cf: Colorf): GradientHints = {
    val hsb = cf.toHSB
    GradientHints(
      hue = hsb(0),
      spectrum = 360,
      saturation = hsb(1),
      brightness = hsb(2),
      minDivs = 100000,
      opacity = cf.a
    )
  }
  implicit def yToDbl(y: Y): Double = y.system.now.calc(y.id)
  implicit def oscToYb(osc: Osc): Yb = osc.y
  implicit def func2Lambda(g: Double => Double)(implicit sys: DynamicSystem): Lambda = new LambdaImpl(sys, g)
  implicit def opToOpt(op: OP) = Option(op)
  implicit def stringToOpt(s: String) = Option(s)
  implicit def intToOption(d: Int) = Option(d)
  implicit def doubleToOption(d: Double) = Option(d)
  implicit def intToDblOption(i: Int): Option[Double] = Option(i)
  implicit def intToFltOption(i: Int): Option[Float] = Option(i)
  implicit def floatToOption(d: Float) = Option(d)
  implicit def booleanToOption(d: Boolean) = Option(d)
  implicit def paramToOption(t: Param) = Option(t)
  implicit def odeToOption(ode: OdeMethod): Option[OdeMethod] = Option(ode)
  implicit def numTypeToOption(nt: NumberType): Option[NumberType] = Option(nt)
  implicit def ybToOption(y: Yb) = Option(y)
  implicit def layerTypeToOption(lt: VariableType) = Option(lt)
  implicit def linkToY(l: Linkable): Y = l.src

  //  DO NOT be tempted to do this.  it will evaluate upon construction
  //  implicit def scalaToProcedure(f : => Any) : Procedure = new ProcedureImpl(f)

  val EMPTY_STRING = ""
  object GREEK {
    val ALPHA = "\u03B1"
    val BETA = "\u03B2"
    val DELTA = "\u03B4"
    val EPSILON = "\u03B5"
    val ETA = "\u03B7"
    val GAMMA = "\u03B3"
    val KAPPA = "\u03BA"
    val LAMDA = "\u03BB"
    val MU = "\u03BC"
    val OMEGA = "\u03C9"
    val PI = "\u03C0"
    val PHI = "\u03C6"
    val RHO = "\u03C1"
    val SIGMA = "\u03C3"
    val TAU = "\u03C4"
    val THETA = "\u03B8"
    val SIGMA_UPPERCASE = "\u03A3"
    // PI 03A0
  }
  object AlwaysGpuSync extends GpuSync {
    def needsSync(tick: Tick, lay: AbstractLayer): Boolean = true
    override def toString: String = "AlwaysGpuSync"
  }
  object ManualGpuSync extends GpuSync {
    def needsSync(tick: Tick, lay: AbstractLayer): Boolean = false
    override def toString: String = "ManualGpuSync"
  }
  object NULL_LAYOUT extends ModelLayout { @inline def layout(): Unit = {} }
  object NULL_TICK_FUNCTION extends TickFunction { @inline def eval(te: Tick) = 0.0 }
  object NULL_TICK_LISTENER extends TickListener { @inline def tick(te: Tick): Unit = {} }
  object NULL_PROCEDURE extends Procedure { @inline def body(): Unit = {} }
  object IDENTITY_FUNCD extends FuncD { def eval(d: Double): Double = d }

  implicit class LinkableIterExt[T <: Iterable[Linkable]](val iter: T) extends AnyVal {
    def -->(tgt: Linkable)(implicit g: DynamicSystem): Iterable[E] = g.sparse.addWs(iter, tgt)
  }
  implicit class LinkableArrayExt[Z <: Linkable](val src: Array[Z]) extends AnyVal {
    def -->(tgt: Linkable)(implicit g: DynamicSystem): Iterable[E] = g.sparse.addWs(src, tgt)
  }
  implicit class EIterExt[IterE <: Iterable[E]](val edges: IterE) extends AnyVal {
    def w: Double = ??? // only here for setter
    def w_=(w: Double): Unit = {
      for (e <- edges) e.w = w
    }
    def tie: TieType = ??? // only here for setter
    def tie_=(tie: TieType)(implicit sys: DynamicSystem) = { for (e <- edges) e.tie = tie }
  }
  implicit class ParamExt(val p: Param) extends AnyVal {
    def tied(implicit sys: DynamicSystem): Iterable[W] = { sys.params.getWs(p) }
  }
  implicit class WExt(val w: W) extends AnyVal {
    def tie(implicit sys: DynamicSystem): Option[Param] = sys.params.getTie(w)
    // NOTE : must call tie = None before change an existing tie, nothing for new
    def tie_=(optT: Option[Param])(implicit sys: DynamicSystem): W = {
      if (optT.isEmpty)
        for (t <- tie)
          sys.params.setTie(w, None)
      else
        sys.params.setTie(w, optT)
      w
    }
  }
  implicit class TimeIntExt(val i: Int) extends AnyVal {
    def s: Int = i * 60
    def m: Int = i * 3600
  }
  implicit class TimeDoubleExt(val i: Double) extends AnyVal {
    def m: Int = (i * 3600.0).toInt
    def toPeriod: Double = Osc.toPeriod(i)
    def toFreq()(implicit sys: DynamicSystem): Double = Osc.toFreq(i, sys.tau)
  }
  implicit class LocatableIter[+It <: Iterable[UIable[ElementUI]]](val locs: It) extends AnyVal {
    def horizontal(loc: Loc, spacing: Double = 100.0): Unit = {
      for ((l, i) <- locs.zipWithIndex) l.ui.loc = loc.right(i * spacing)
    }
    def center(loc: Loc, spacing: Double = 100.0): Unit = {
      val len: Double = locs.size
      val start = if (len % 2 == 0) -spacing * (len - 1.0) / 2.0 else -spacing * Math.floor(len / 2)
      for ((l, i) <- locs.zipWithIndex) l.ui.loc = loc.right(start + i * spacing)
    }
  }
  implicit class LocatableArray[H <: UIable[ElementUI]](val locs: Array[H]) extends AnyVal {
    def horizontal(loc: Loc, spacing: Double = 100.0): Unit = {
      for ((l, i) <- locs.zipWithIndex) l.ui.loc = loc.right(i * spacing)
    }
    def center(loc: Loc, spacing: Double = 100.0): Unit = {
      val len: Double = locs.size
      val start = if (len % 2 == 0) -spacing * (len - 1.0) / 2.0 else -spacing * Math.floor(len / 2)
      for ((l, i) <- locs.zipWithIndex) l.ui.loc = loc.right(start + i * spacing)
    }
  }
  //
  // math/breeze section
  //
  val normal = Gaussian(0.0, 1.0)
  implicit val DEFAULT_PRECISON = Precision(0.001)
  implicit def dmToOption(dm: DenseMatrix[Double]) = Option(dm)
  implicit def dmToOpt(dm: DenseMatrix[Float]): Option[DenseMatrix[Float]] = Option(dm)
  def NORMAL(n: Int) = DenseVector.rand[Double](size = n, rand = normal)
  def ZEROS(n: Int) = DenseVector.zeros[Double](n)

  implicit class BreezeIntExt(val to: Int) extends AnyVal {
    import breeze.linalg._
    def ::(from: Int): DenseVector[Double] = {
      val len = to - from
      linspace(from, to, len + 1)
    }
  }
  implicit class DenseMatrixFExt(val dm: DenseMatrix[Float]) extends AnyVal {
    def clear(): Unit = {
      val len = dm.data.length
      var i = 0
      while (i < len) {
        dm.data(i) = 0.0f
        i += 1
      }
    }

    /** here for BLAS */
    def transposeString: String = if (dm.isTranspose) "T" else "N"
  }
  implicit class DenseMatrixExt(val dm: DenseMatrix[Double]) extends AnyVal {
    def clear(): Unit = {
      val len = dm.data.length
      var i = 0
      while (i < len) {
        dm.data(i) = 0.0
        i += 1
      }
    }
    // BLAS
    def transposeString: String = if (dm.isTranspose) "T" else "N"
  }
  implicit def matMul2Inf(matMul: MatMul): Influence = matMul.toInfluence
  implicit class MatMulExt[T](val matMul: MatMul) extends AnyVal {
    def toInfluence: Influence = new MatMulInfluence(matMul)
  }
  implicit def dot2Inf(dot: Dot): Influence = dot.toInfluence
  implicit class DotExt[T](val dot: Dot) extends AnyVal {
    def toInfluence: Influence = new DotInfluence(dot)
  }
  implicit class TgtExt[T <: Tgt](val src: T) extends AnyVal {
    def chain(ffTie: Param)(implicit g: DynamicSystem): LambdaLink = src >>> (src, new ChainImpl(ffTie))
    def chainBackwards(ffTie: Param)(implicit g: DynamicSystem): LambdaLink = src >>> (src, new ChainBackwards(ffTie))

    /** NOTE: only supports odd sized kernels */
    def conv1D(kernel: Array[Double], ktype: KernelType = KernelType.Wrap)(implicit g: DynamicSystem): Conv1D =
      Conv1D(src, kernel, src)
  }

  implicit def den2array[T](den: DenseVector[T]): Array[T] = den.data
  implicit class DenseVectorExt(val dv: DenseVector[Double]) extends AnyVal {
    import breeze.linalg._
    def clear(): Unit = {
      val len = dv.data.length
      var i = 0
      while (i < len) {
        dv.data(i) = 0.0
        i += 1
      }
    }
    def normalize(): DenseVector[Double] = {
      val total = sum(dv)
      dv :/= total
    }
  }
  implicit class DenseVectorFExt(val dv: DenseVector[Float]) extends AnyVal {
    import breeze.linalg._
    def clear(): Unit = {
      val len = dv.data.length
      var i = 0
      while (i < len) {
        dv.data(i) = 0.0f
        i += 1
      }
    }
    def normalize(): DenseVector[Float] = {
      val total = sum(dv)
      dv :/= total
    }
  }
  private class RandWrapperImpl(dist: breeze.stats.distributions.Rand[Double])
      extends breeze.stats.distributions.Rand[Float] {
    def draw(): Float = dist.draw().asInstanceOf[Float]
  }
  implicit def randDtorandF(randD: breeze.stats.distributions.Rand[Double]): breeze.stats.distributions.Rand[Float] =
    new RandWrapperImpl(randD)

  import KernelType._
  def chain(data: Iterable[Y], w: Double, ktype: KernelType = KernelType.Wrap)(implicit mn: DynamicSystem): TieType = {
    chain(data, Param(name = "chain", max = 2.0, init = w), ktype)
  }
  def chain(data: Iterable[Y], tie: TieType, ktype: KernelType)(implicit mn: DynamicSystem): TieType = {
    for (win <- data.sliding(2))
      win.head --> win.last tie = tie

    if (ktype == Wrap) data.last --> data.head tie = tie
    tie
  }
  def kernel(data: Iterable[Y], ktype: KernelType, kernel: Double*)(implicit mn: DynamicSystem): Unit = {
    val ci = kernel.length
    val iter =
      if (ktype == Wrap) data.iterator ++ data.slice(from = 0, until = 2 * kernel.length).iterator else data.iterator
    val klength = 2 * kernel.length + 1 // sym kernel only encodes one side...
    val indices = 0 until kernel.length
    for (win <- iter.sliding(klength)) {
      val center = win(ci)
      for (i <- indices) {
        val w = kernel(i)
        val off = i + 1
        center --> win(ci + off) w = w
        center --> win(ci - off) w = w
      }
    }
  }
  import breeze.linalg._
  import breeze.numerics._
  def gaussNorm(range: DenseVector[Double], sigma: Double) = gauss(range, sigma).normalize()
  def gauss(range: DenseVector[Double], sigma: Double) = {
    val sigma_sq = sigma * sigma
    exp((range ^:^ 2.0 :/= sigma_sq) :*= -0.5)
  }
  @inline def euclid3D(x: Double, y: Double, z: Double, x2: Double, y2: Double, z2: Double) = {
    val xD = x - x2
    val yD = y - y2
    val zD = z - z2
    sqrt(xD * xD + yD * yD + zD * zD)
  }
  @inline def euclid(x: Double, y: Double, x2: Double, y2: Double) = sqrt(pow(x - x2, 2.0) + pow(y - y2, 2.0))
  def rbf(basis: Double, y: Double, radius: Double = 1.0, min: Double = 0.1) = {
    val res = exp(-pow(basis - y, 2.0) / (radius * radius))
    if (res > min) res else 0.0
  }

  def rbfFunc(f: => Double, radius: Double = 1.0, min: Double = 0.1) = {
    val res = exp(-pow(f, 2.0) / (radius * radius))
    if (res > min) res else 0.0
  }

  // UI
  implicit def yToLoc(y: Y): Loc = y.ui.loc
  implicit def locToOption(loc: Loc): Option[Loc] = Option(loc)
  implicit class VizExt(val viz: Viz) extends AnyVal {
    def <(other: Viz): Boolean = viz.ordinal() < other.ordinal()
    def >(other: Viz): Boolean = viz.ordinal() > other.ordinal()
    def >=(other: Viz): Boolean = viz.ordinal() >= other.ordinal()
    def <=(other: Viz): Boolean = viz.ordinal() <= other.ordinal()
  }
  implicit class LocatableExt[T <: Locatable](val src: T) extends AnyVal {
    def horizontal[L <: Locatable](loc: Loc, spacing: Int, locs: L*): Unit = {
      src.loc = loc
      for ((l, i) <- locs.zipWithIndex) l.loc = loc.right((i + 1) * spacing)
    }
  }

  // language helpers
  implicit class AnyRefExt[T <: AnyRef](val _thiz: T) extends AnyVal {
    def as[S](implicit man: Manifest[S]): Option[S] = _thiz match {
      case s: S => Some(s)
      case _    => None
    }
    def ?(default: T) = if (_thiz != null) _thiz else default
  }
  implicit class StringNullExt(val f: String) extends AnyVal {

    /** NOTE : if using syntactic sugar, do expressions in parens, e.g., name ? ("def"+1) */
    def ?(default: String): String = if (f != null && !f.isEmpty) f else default
  }
  implicit class SeqExt[T](val seq: scala.collection.Seq[T]) extends AnyVal {
    def randomPick: T = seq(Random.nextInt(seq.length))
    def randomPicks(n: Int = 5): IndexedSeq[T] = for (i <- 0 until n) yield randomPick
  }
  implicit class DoubleArrayExt(val array: Array[Double]) extends AnyVal {
    def pretty(): String = { array.iterator.map("%.3f".format(_)).mkString(", ") }
  }
  implicit final class OptionExt[T](val opt: Option[T]) extends AnyVal {
    @inline def getOrElseP(default: T): T = if (opt.isDefined) opt.get else default
  }
  implicit final class OptionExtD(val opt: Option[Double]) extends AnyVal {
    @inline def getOrElseP(default: Double): Double = if (opt.isDefined) opt.get else default
  }
  implicit final class OptionExtF(val opt: Option[Float]) extends AnyVal {
    @inline def getOrElseP(default: Float): Float = if (opt.isDefined) opt.get else default
  }
  implicit class IterableExt[T <: Any](val iter: Iterable[T]) extends AnyVal {
    def contains[T](ele: T): Boolean = iter.exists(_ == ele)
  }
  implicit class IteratorExt[T <: Any](val iter: Iterator[T]) extends AnyVal {
    def headOption: Option[T] = if (iter.hasNext) Some(iter.next()) else None
    def contains[T](ele: T): Boolean = iter.exists(_ == ele)
  }
  implicit class IntExt(val i: Int) extends AnyVal {
    def even = i % 2 == 0
    def odd = !even
    def toEven: Int = if (i % 2 == 0) i else i + 1

    /** +/- range with this int as middle */
    def pick(range: Int = 2): Int = {
      val r = Math.random()
      i + Math.signum(range * (r - 0.5)).toInt * Math.round(r * range).toInt
    }
  }
  case class Precision(val p: Double)
  implicit class DoubleExt(val d: Double) extends AnyVal {
    def ~(d2: Double)(implicit p: Precision): Boolean = Math.abs(d - d2) < p.p
    def !~(d2: Double)(implicit p: Precision): Boolean = Math.abs(d - d2) >= p.p
  }
  implicit class FloatExt(val d: Float) extends AnyVal {
    def ~(d2: Float)(implicit p: Precision): Boolean = Math.abs(d - d2) < p.p
    def !~(d2: Double)(implicit p: Precision): Boolean = Math.abs(d - d2) >= p.p
  }
  private[memnets] final class ProcedureImpl(f: => Unit) extends Procedure { @inline def body() = { f } }
}
