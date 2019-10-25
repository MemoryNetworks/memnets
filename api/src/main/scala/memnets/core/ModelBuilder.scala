package memnets.core

import memnets.core.impl.ModelImpl
import memnets.model._

import scala.beans.BeanProperty

object ModelBuilder {

  def apply(body: Model => Any): ModelBuilder = {
    apply()(body)
  }
  def apply(
      name: String = EMPTY_STRING,
      branch: SciBranch = SciBranch.Mathematics,
      modelType: ModelType = ModelType.Sci,
      tags: Iterable[String] = List(),
      author: Profile = MemNetsProfile,
      desc: String = EMPTY_STRING,
      topic: Option[Topic] = None,
      library: Option[Library] = None,
      hints: ModelHints = ModelHints()
  )(body: Model => Any): ModelBuilder = {

    val bld = new ModelBuilder(name, branch, modelType, tags, author, desc, topic, hints)(body)
    for (lib <- library)
      lib._builders += bld
    bld
  }
  // Java
  type JF = java.util.function.Consumer[JModel]
  def create(name: String, hints: ModelHints)(body: JF): ModelBuilder = {
    ModelBuilder(
      name = name,
      hints = hints
    )(c => body.accept(new JModel(c)))
  }
  def create(name: String, modelType: ModelType)(body: JF): ModelBuilder = {
    ModelBuilder(
      name = name,
      modelType = modelType
    )(c => body.accept(new JModel(c)))
  }
  def create(name: String)(body: JF): ModelBuilder = {
    ModelBuilder(
      name = name
    )(c => body.accept(new JModel(c)))
  }
  def create(body: JF): ModelBuilder = create("")(body)
}

class ModelBuilder(
    @BeanProperty var name: String,
    @BeanProperty var branch: SciBranch,
    @BeanProperty var modelType: ModelType,
    @BeanProperty var tags: Iterable[String],
    @BeanProperty var author: Profile,
    @BeanProperty var description: String,
    @BeanProperty var topic: Option[Topic],
    @BeanProperty var hints: ModelHints
)(body: Model => Any)
    extends Dsl
    with Logging {

  def build(cfg: ModelConfig = new ModelConfig): BuiltModel = {
    require(cfg != null)
    if (hints != null) cfg.hints = hints

    val model = new ModelImpl(this, cfg)
    body(model)
    // always add trial with no inputs for user
    // also covers case where no Trial or Inputs were created
    if (!model.system.game) Trial(name = "no inputs")(model)
    model
  }
  override def toString: String = name
}
