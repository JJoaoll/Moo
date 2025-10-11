fun safeDiv(a: Int, b: Int) -> Option(Int) do
  match b == 0 with
    case True do
      return None
    end-case
    case False do
      return Some(a / b)
    end-case
  end-match
end-safeDiv

fun main() -> Int do
  let result1: Option(Int) := safeDiv(10, 2) 
  let result2: Option(Int) := safeDiv(10, 0)

  print(result1)
  print(result2)

  return 0
end-main