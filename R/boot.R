# Bootstrap 
boot = function(
  estimand,
  B = 10000,
  FUN,
  ...
) {
  list2env(list(...), environment())
  theta_b = sapply(
    1:B,
    function(b) {
      # Simulate data:
      X = FUN(...)
      # Estimate:
      theta_sim = eval(parse(text=estimand))
      return(theta_sim)
    }
  )
  return(theta_b)
}