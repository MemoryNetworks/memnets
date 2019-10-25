package memnets.ui

trait SkinFactory[SkinType] {
  def create(): SkinType
}
