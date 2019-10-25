package memnets.ui

import memnets.model._

case class Face(pt1: Int, pt2: Int, pt3: Int)

final class MeshSurface(val grid: GridData, val colorMap: ColorMap[_]) extends Tickable with Logging {
  // have 1 pt buffer on each side

  logger.debug(s"mesh rows: ${grid.rows}, cols: ${grid.cols}")
  val rows2 = grid.rows + 2
  val cols2 = grid.cols + 2
  val faces = Array.ofDim[Face](2 * (rows2 - 1) * (cols2 - 1))
  val faceArray = new Array[Int](6 * faces.length)
  val pointArray = new Array[Float](3 * rows2 * cols2)

  /** need to call only ONCE, unlike Tickable.init where diff Skins can call again */
  def meshInit(): Unit = {
    val rStart = -rows2 / 2.0f
    val cStart = -cols2 / 2.0f
    var i = 0
    for {
      y <- 0 until cols2
      x <- 0 until rows2
    } {
      val off = 3 * i
      pointArray(off) = rStart + x.toFloat
      pointArray(off + 1) = cStart + y.toFloat
      i += 1
    }

    i = 0
    for {
      c <- 0 until (cols2 - 1)
      r <- 0 until (rows2 - 1)
    } {

      // compute indices for points and texture coordinates
      val p00 = c * rows2 + r
      val p01 = p00 + 1
      val p10 = p00 + rows2
      val p11 = p10 + 1
      // define faces (first triangle)
      faces(i) = Face(p00, p10, p11)
      i += 1
      // define faces (second triangle)
      faces(i) = Face(p11, p01, p00)
      i += 1
    }

    val flen = faces.length
    i = 0
    while (i < flen) {
      val f = faces(i)
      val off = i * 6
      faceArray(off) = f.pt1
      faceArray(off + 2) = f.pt2
      faceArray(off + 4) = f.pt3
      i += 1
    }
  }
  @inline private def apply(i: Int) = pointArray(3 * i + 2)
  @inline private def update(i: Int, z: Float): Unit = pointArray(3 * i + 2) = z
  override def tick(te: Tick): Unit = {
    val rows = grid.rows
    val cols = grid.cols
    var c = 0
    while (c < cols) {
      // have 1 pt buffer on each side
      val off = 1 + (c + 1) * rows2
      var r = 0
      while (r < rows) {
        this(r + off) = grid.act(te, r, c).asInstanceOf[Float]
        r += 1
      }
      c += 1
    }
    updateMeshFaces()
  }
  protected def updateMeshFaces(): Unit = {
    val len = faces.length
    var i = 0
    while (i < len) {
      val f = faces(i)
      val off = i * 6
      faceArray(off + 1) = colorMap.index(apply(f.pt1))
      faceArray(off + 3) = colorMap.index(apply(f.pt2))
      faceArray(off + 5) = colorMap.index(apply(f.pt3))
      i += 1
    }
  }
  def faceSmoothingGroups() = {
    val smoothGrps: Array[Int] = new Array[Int](faces.length) // if use length 0 == hard edges
    java.util.Arrays.fill(smoothGrps, 1) // 1: soft edges, all the faces in same surface
    smoothGrps
  }
  meshInit()
}
