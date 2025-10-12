@global globalrate: Float := 0.05

fun calculateinterest(principal: Float) -> Float do
  return principal *. @globalrate
end-calculateinterest

fun main() -> Float do
  let mysavings: Float := 1000.0
  let interestearned: Float := calculateinterest(mysavings)
  return mysavings +. interestearned +. @globalrate
end-main