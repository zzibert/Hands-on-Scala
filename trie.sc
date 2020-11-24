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
  def prefixesMatchingString(s: String): Set[String] = {
    prefixesMatching0(s).map(s.substring(0, _))
  }
  def stringsMatchingPrefix(s: String): Set[String] = {
    var current = Option(root)
    for (c <- s if current.nonEmpty) current = current.get.children.get(c)
    if (current.isEmpty) Set()
    else {
      val output = Set.newBuilder[String]
      def recurse(current: Node, path: List[Char]): Unit = {
        if (current.HasValue) output += (s + path.reverse.mkString)
        for ((c, n) <- current.children) recurse(n, c :: path)
      }
      recurse(current.get, Nil)
      output.result()
    }
  }
}