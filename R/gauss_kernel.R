gauss_kernel <- function(x,mu,s=1) {
  exp((-(x-mu)^2)/(2*s^2))
}
