logit_irls <- function(X, y, beta_0=NULL, tau=1e-9, max_iter=10000) {
  if(!all(X[,1]==1)) {
    X <- cbind(1,X)
  }
  p <- ncol(X)
  n <- nrow(X)
  # Initialization: ----
  if (is.null(beta_0)) {
    beta_latest <- matrix(rep(0, p)) # naive first guess
  }
  W <- diag(n)
  can_still_improve <- T
  iter <- 1
  # Iterative reweighted least-squares (IRLS):
  while(can_still_improve & iter < max_iter) {
    y_hat <- X %*% beta_latest
    p_y <- exp(y_hat)/(1+exp(y_hat))
    df_latest <- crossprod(X,y-p_y) # gradient
    diag(W) <- p_y*(1-p_y)
    Z <- X %*% beta_latest + qr.solve(W) %*% (y-p_y)
    beta_latest <- qr.solve(crossprod(X,W%*%X),crossprod(X,W%*%Z))
    can_still_improve <- mean(abs(df_latest))>tau # convergence reached?
    iter <- iter + 1
  }
  return(
    list(
      fitted = p_y,
      coeff = beta_latest
    )
  )
}