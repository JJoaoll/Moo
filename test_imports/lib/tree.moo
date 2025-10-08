type Tree(a) def
  Leaf(a)
  Node(Tree(a), a, Tree(a))
end-def

fun treeSize(tree: Tree(Int)) -> Int do
  match tree with
    case Leaf(_) do 
      return 1
    end-case
    case Node(left, _, right) do 
      return (treeSize(left) + 1 + treeSize(right))
    end-case
  end-match
end-treeSize
