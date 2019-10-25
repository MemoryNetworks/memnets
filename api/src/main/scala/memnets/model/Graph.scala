package memnets.model

trait Graph[N, E] {
  def nodes: Iterable[N]
  def loop(n: N): Option[E] = find(n, n)
  def find(src: N, tgt: N): Option[E]
  def inEdges(n: N): Iterator[E]
  def outEdges(n: N): Iterator[E]
}

trait MutableGraph[N, E] extends Graph[N, E] {
  def add(n: N): Unit
  def remove(n: N): Unit
  def addEdge(src: N, tgt: N): E
  def removeEdge(e: E): Unit
}
