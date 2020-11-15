UNIF <- function(X, y, m, weighted=F, rand_state=NULL) {
  if (!is.null(rand_state)) {
    set.seed(rand_state)
  }
  indices <- sample(1:n, size=m)
  X_m <- X[indices,]
  y_m <- y[indices]
  beta_hat <- qr.solve(X_m, y_m)
  y_hat <- c(X %*% beta_hat)
  return(
    list(
      fitted = y_hat,
      coeff = beta_hat
    )
  )
}