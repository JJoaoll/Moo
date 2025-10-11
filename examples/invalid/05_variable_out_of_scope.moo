fun ipisilon() -> Int do
  let y: Int := 4
  return y
end-ipisilon

fun main() -> Int do
  let x: Int := 5

  // ERRO: A variável 'y' não existe neste escopo.
  return x + y
end-main