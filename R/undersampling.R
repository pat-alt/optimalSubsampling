# Undersampler class ----
undersampler <- function(X, y) {
  distinct_values <- unique(y)
  if(length(distinct_values)>2) {
    stop("y should have 2 distinct levels.")
  }
  grouped <- lapply(
    distinct_values, 
    function(i) {
      indices <- which(y==i)
      n <- length(indices)
      list(indices=indices, n=n)
    }
  )
  min_class <- which.min(sapply(grouped, function(i) i[[2]]))
  n_min <- grouped[[min_class]]$n
  indices_min <- grouped[[min_class]]$indices
  n_maj <- grouped[[-min_class]]$n
  indices_maj <- grouped[[-min_class]]$indices
  v <- list(
    X = X,
    y = y,
    n = n_min + n_maj,
    n_min = n_min,
    indices_min = indices_min,
    n_maj = n_maj,
    indices_maj = indices_maj
  )
  class(v) <- "undersampler"
  return(v)
}

# UNIF method: ----
UNIF.undersampler <- function(vars, weighted=F, rand_state=NULL, fit_model=T) {
  if (!is.null(rand_state)) {
    set.seed(rand_state)
  }
  invisible(list2env(vars, envir = environment()))
  indices <- sample(indices_maj, size=n_min) # sample from majority class
  indices <- c(indices_min, indices)
  X_m <- X[indices,]
  y_m <- y[indices]
  weights <- NULL
  if (fit_model) {
    beta_hat <- glm(y_m~X_m,family = "binomial")$coefficients
    if(!all(X[,1]==1)) {
      X <- cbind(1,X)
    }
    y_hat <- c(X %*% beta_hat)
    p_y <- exp(y_hat)/(1+exp(y_hat))
    return(
      list(
        X_m = X_m,
        y_m = y_m,
        linear_predictors = y_hat,
        fitted = p_y,
        coeff = beta_hat
      )
    )
  } else {
    return(
      list(
        X = X_m,
        y = y_m,
        weights = weights
      )
    )
  }
  
}

UNIF = function(vars, weighted=F, rand_state=NULL, fit_model=T) {
  UseMethod("UNIF")
}

# mVc:
mVc.undersampler <- function(vars, weighted=F, rand_state=NULL, fit_model=T) {
  invisible(list2env(vars, envir = environment()))
  # 1.) Step:
  output <- UNIF(vars)
  fitted_unif <- output$fitted
  abs_dev <- abs(y-fitted_unif)
  X_unif <- output$X
  y_unif <- output$y
  # 2.) Step:
  # Euclidian norms:
  predictor_len <- sqrt(X**2 %*% rep(1,ncol(X)))
  prob <- (abs_dev * predictor_len) / crossprod(abs_dev,predictor_len)[1]
  # Sample:
  indices <- sample(
    x = indices_maj, 
    size = n_min,
    replace = T,
    prob = prob[indices_maj]
  )
  indices <- c(indices_min, indices)
  X_m <- rbind(X[indices,],X_unif)
  y_m <- c(y[indices],y_unif)
  weights <- c(1/prob[indices],rep(n,length(y_unif)))
  if (fit_model) {
    # Fit:
    if (weighted) {
      beta_hat <- glm(y_m~X_m,family = "binomial", weights = weights)$coefficients
    } else {
      beta_hat <- glm(y_m~X_m,family = "binomial")$coefficients
    }
    # Predict:
    if(!all(X[,1]==1)) {
      X <- cbind(1,X)
    }
    y_hat <- c(X %*% beta_hat)
    p_y <- exp(y_hat)/(1+exp(y_hat))
    return(
      list(
        linear_predictors = y_hat,
        fitted = p_y,
        coeff = beta_hat
      )
    )
  } else {
    return(
      list(
        X = X_m,
        y = y_m,
        weights = weights
      )
    )
  }
}

mVc = function(vars, weighted=F, rand_state=NULL, fit_model=T) {
  UseMethod("mVc")
}