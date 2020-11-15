extract_features = function(x,mu,s,const=T) {
  Phi_k <- sapply(
    1:length(mu),
    function(p) {
      mu_p <- mu[p]
      gauss_kernel(x=x, mu=mu_p, s = s)
    }
  )
  if (!is.matrix(Phi_k)) {
    Phi_k <- matrix(Phi_k, ncol=length(Phi_k))
  }
  if (const) {
    Phi_k <- cbind(
      rep(1,nrow(Phi_k)),
      Phi_k
    )
  }
  return(Phi_k)
}