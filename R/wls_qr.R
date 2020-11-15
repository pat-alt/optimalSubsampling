wls_qr <- function(X, y, weights) {
  Phi <- diag(weights)
  beta <- qr.solve(t(X) %*% Phi %*% X, t(X) %*% Phi %*% y)
  return(beta)
}