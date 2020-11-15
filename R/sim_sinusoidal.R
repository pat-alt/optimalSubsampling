sim_sinusoidal = function(n=25,a=0,b=1,sigma=0.3) {
  x_star = sort(runif(n,a,b))
  y_star = sinusoidal(x_star) + rnorm(n,sd=sigma)
  return(
    list(
      y_star = y_star,
      x_star = x_star
    )
  )
}
