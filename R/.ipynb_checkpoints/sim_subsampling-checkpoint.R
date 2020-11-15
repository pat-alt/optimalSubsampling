sim_subsampling <- function(Phi, y, m, subsample_estimator, J=1000, bias_correct=F, B=1000, ...) {
  # Compute full sample OLS estimator:
  y_hat_ols <- qr.fitted(qr.default(Phi),y)
  # Compute J predictions from subsample estimator:
  run_J_predictions = function() {
    output <- rbindlist(
      lapply(
        1:J,
        function(j) {
          estimate <- tryCatch(
            subsample_estimator(Phi, y, m, ...), # subsample estimate
            error = function(e) {
              return(list(fitted=NA))
            }
          )
          if (bias_correct) { 
            # Bootstrap bias-correction term:
            w <- 0
          } else {
            w <- 0
          }
          data.table(
            y_hat_ols = c(y_hat_ols),
            y_hat_subsample = estimate$fitted,
            i = 1:length(y_hat_ols),
            j = j,
            w = w
          )
        }
      )
    )
    # Compute variance, bias and MSE:
    output <- na.omit(output) # for cases where estimation yielded NaNs (can happen when m->p)
    output[,avg_y_hat_subsample := mean(y_hat_subsample), by=.(i)]
    V <- output[,.(V_b=(1/n)*norm(as.matrix(y_hat_subsample - avg_y_hat_subsample), type="f")^2),by=.(j)][,mean(V_b)]
    bias_sq <- output[,.(avg_bias_i = mean(y_hat_ols - y_hat_subsample)), by=.(i)][,(1/n)*norm(as.matrix(avg_bias_i, type="f")^2)]
    mse_check <- output[,.(mse_b=(1/n)*norm(as.matrix(y_hat_ols - y_hat_subsample), type="f")^2),by=.(j)][,mean(mse_b)]
    mse <- V + bias_sq
    if (abs(mse_check-mse)>1e-5) {
      warning(
        sprintf(
          'Inconsistent values for MSE and MSE from decomposition: %0.5f', 
          mse_check-mse
        )
      )
    }
    output <- data.table(
      value = c(V,bias_sq,mse),
      variables = c("V", "Squared bias", "MSE")
    )
  }
  # Run with tryCatch:
  output <- tryCatch(
    run_J_predictions(),
    error=function(e) {
      warning("Error occured. Returning NaNs.")
      output <- data.table(
        value = rep(NA, 3),
        variables = c("V", "Squared bias", "MSE")
      )
      return(output)
    }
  )
  return(
    output
  )
}