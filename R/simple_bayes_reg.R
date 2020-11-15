simple_bayes_reg = function(Phi,lambda,y) {
  beta_hat <- regularized_ls(Phi=Phi_k,y=y_k,lambda=lambda) # fit model on (y,x)
  Sigma_posterior <- solve(1/v_b * diag(ncol=ncol(Phi), nrow=ncol(Phi)) + 1/v * crossprod(Phi) )
  return(
    list(
      beta = beta_hat,
      Sigma = Sigma_posterior
    )
  )
}