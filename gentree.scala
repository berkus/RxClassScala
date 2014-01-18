trait Tree
case class Inner(left: Tree, right: Tree) extends Tree
case class Leaf(x: Int) extends Tree

val integers = new Generator[Int] {
    def generate = scala.util.Random.nextInt()
}

val booleans = integers.map(_ >= 0)

def leafNode: Generator[Leaf] = for {
    value <- integers
} yield Leaf(value)

def innerNode: Generator[Inner] = for {
    left <- trees
    right <- trees
} yield InnerNode(left, right)

def trees: Generator[Tree] = for {
    isLeaf <- booleans
    tree <- if (isLeaf) leafNode else innerNode
} yield tree

trees.generate
