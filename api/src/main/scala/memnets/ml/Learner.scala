package memnets.ml

trait Learner {
  def classify(pt: Row): Boolean
  def name: String
  def train(data: Data): Unit
}
