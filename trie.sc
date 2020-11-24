class Trie() {
  class Node(var HasValue: Boolean,
            var children: collection.mutable.Map[Char, Node] = collection.mutable.Map())
  val root = new Node(false)
  def add(s: String) = {
    var current = root
    for (c <- s) current = current.children.getOrElseUpdate(c, new Node(false))
    current.HasValue = true
  }
  def contains(s: String): Boolean = {
    var current = Option(root)
    for (c <- s if current.nonEmpty) current = current.get.children.get(c)
    current.exists(_.HasValue)
  }
  def prefixesMatching0(s: String): Set[Int] = {
    var current = Option(root)
    val output = Set.newBuilder[Int]
    for ((c, i) <- s.zipWithIndex if current.nonEmpty) {
      if (current.get.HasValue) output += i
      current = current.get.children.get(c)
    }
    if (current.exists(_.HasValue)) output += s.length
    output.result()
  }
}