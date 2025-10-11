fun factorial(n: Int) -> Int do
  match n with
    case 0 do
      return 1
    end-case
    case _ do
      return n * factorial(n - 1)
    end-case
  end-match
end-factorial

fun main() -> Int do
  return factorial(5)
end-main