package memnets.model

import memnets.linalg.NumberType
import memnets.model.impl._

trait LinkableLayer {
  def src: AbstractLayer
  def srcSys: DynamicSystem = src.system
  def -->(tgt: LinkableLayer): OnetoOne = {
    if (src == tgt.src && src.isInstanceOf[Layer] && tgt.src.isInstanceOf[Layer])
      srcSys.addLink(new DecayImpl(src.asInstanceOf[Layer], tgt.src.asInstanceOf[Layer]), srcOut = false)
    else
      srcSys.addLink(new OnetoOneImpl(src, tgt.src))
  }
  def ==>(tgt: LinkableLayer): DenseLink =
    srcSys.addLink(
      if (srcSys.numberType == NumberType.Doubles) new DenseLinkImpl(src, tgt.src)
      else new DenseLinkFImpl(src, tgt.src))
  def -->(tgt: Linkable): DotTied = srcSys.addLink(new DotTiedImpl(src, tgt.src))
  def -:>(tgt: Linkable): Dot =
    srcSys.addLink(
      if (srcSys.numberType == NumberType.Doubles) new DotImpl(src, tgt.src) else new DotFImpl(src, tgt.src))

  /** NOTE : column-major */
  def -:>(tgt: LinkableLayer): MatMul = {
    srcSys.addLink(
      if (srcSys.numberType == NumberType.Doubles) new MatMulDImpl(src, tgt.src) else new MatMulFImpl(src, tgt.src))
  }
  def -%>(tgt: LinkableLayer): SparseLink = {
    srcSys.addLink(
      if (srcSys.numberType == NumberType.Doubles) new SparseLinkDImpl(src, tgt.src)
      else new SparseLinkFImpl(src, tgt.src))
  }
  def >>>(tgt: LinkableLayer, lambda: LambdaCalc): LambdaLink = srcSys.addLink(new LambdaLinkImpl(src, tgt.src, lambda))
  // Java
  def linkTo(tgt: LinkableLayer): OnetoOne = -->(tgt.src)
  def linkTo(tgt: LinkableLayer, w: Double): OnetoOne = -->(tgt.src).setW(w)
  def linkDTo(tgt: LinkableLayer): DenseLink = ==>(tgt.src)
  def linkTo(tgt: Linkable): DotTied = -->(tgt.src)
  def dot(tgt: Linkable): Dot = -:>(tgt.src)
  def matMul(tgt: LinkableLayer): MatMul = -:>(tgt.src)
  def sparse(tgt: LinkableLayer): SparseLink = -%>(tgt.src)
  def lambda(tgt: LinkableLayer, lambda: LambdaCalc): LambdaLink = >>>(tgt.src, lambda)
}
