regularized_ls = function(Phi, y, lambda, ...) {
  solve(lambda * diag(ncol(Phi)) + crossprod(Phi), crossprod(Phi,y), ...)
}
