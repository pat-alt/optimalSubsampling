# Simulate Binomial(n,p) through inverse CDF method:
sim_binomial = function(n,n_trial,p=0.5,U=NULL) {
  # 1) Simulate uniformly distributed data:
  if (is.null(U)) {
    U = runif(n)
  }
  # 2) Apply inverse CDF:
  X = sapply(
    U,
    function(u) {
      stats::qbinom(u, size=n_trial, prob=p) # there is no closed-form solution for the quantile function of the Binomial
    }
  )
  return(X)
}