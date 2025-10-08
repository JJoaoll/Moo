// Arquivo de teste para demonstrar as mensagens de erro

type Option(a) def
  None
  Some(a)
end-def

fun testErrors() -> Int do
  // Erro 1: Variável não declarada
  let x: Int := y + 1
  
  // Erro 2: Tipo incorreto em atribuição
  x := True
  
  // Erro 3: Declaração duplicada
  let x: Int := 5
  
  // Erro 4: Global não existe
  print(UNDEFINED_GLOBAL)
  
  // Erro 5: Função não existe
  undefinedFunction(1, 2)
  
  // Erro 6: Tipo incompatível em return
  return True
end-testErrors

fun main() -> Int do
  return 0
end-main
