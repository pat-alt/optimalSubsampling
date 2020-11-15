OPT <- function(X, y, m, weighted=F, rand_state=NULL, plot_wgts=F, prob_only=F) {
  n <- nrow(X)
  # Leverage scores:
  svd_X <- svd(X)
  U <- svd_X$u
  H <- tcrossprod(U)
  h <- diag(H)
  # Euclidian norms:
  predictor_len <- sqrt(X**2 %*% rep(1,ncol(X)))
  # Optimal sampling probabilities:
  prob <- (sqrt(1-h) * predictor_len) / crossprod(sqrt(1-h),predictor_len)[1]
  # Plot:
  if (plot_wgts) {
    plot(prob, t="l", ylab="Sampling probability")
  }
  # Output:
  if (prob_only) {
    return(prob)
  } else {
    indices <- sample(
      x = 1:n, 
      size = m,
      replace = T,
      prob = prob
    )
    X_m <- X[indices,]
    y_m <- y[indices]
    weights <- 1/prob[indices]
    if (weighted) {
      beta_hat <- wls_qr(X_m, y_m, weights)
    } else {
      beta_hat <- qr.solve(X_m, y_m)
    }
    y_hat <- c(X %*% beta_hat)
    return(
      list(
        fitted = y_hat,
        coeff = beta_hat,
        prob = prob
      )
    )
  }
}