# Exports main function ``dgp`` for the simulation of data in a heterogeneous
# treatment effect setting.

suppressMessages(library("assertthat"))


dgp <- function(version, n, d, seed=0, noise_sd=1, testing=FALSE) {
  #' Return simulated data for various data generating processes.
  #'
  #' @param version: String determining which dgp is used. Implemented dgp's are
  #' {'complex', 'boundary', 'unbalanced', 'simple'}.
  #' @param n: Positive integer representing the number of individuals to 
  #' sample.
  #' @param d: Positive integer representing the feature dimension. Has to be
  #' greater or equal to 2.
  #' @param seed: Integer representing the random number generator seed. Default
  #' is 0.
  #' @param noise_sd: Positive float representing the standard deviation of the
  #' errors. Default is 1.
  #' 
  #' @return data: a list containing the feature matrix (X), the treatment 
  #' indicator (W), the outcome (Y) and the treatment effect (treatment_effect).
  #' 
  #' @examples
  #' data <- dgp("complex", n=1000, d=20)
  
  # assert inputs
  assertthat::assert_that(is.count(n))
  assertthat::assert_that(is.count(d))
  assertthat::assert_that(is.number(seed))
  assertthat::assert_that(noise_sd > 0)
  
  set.seed(seed)
  
  data <- switch (version,
    "complex" = dgp_complex(n, d, noise_sd),
    "boundary" = dgp_boundary(n, d, noise_sd, testing),
    "unbalanced" = dgp_unbalanced(n, d, noise_sd),
    "simple" = dgp_simple(n, d, noise_sd),
    stop(sprintf("%s is not in set of implemented dgps: {'complex',
                 'boundary', 'unbalanced', 'simple'}", version))
  )
  return(data)
}


#-----------------------------Specific processes-------------------------------

dgp_complex <- function(n, d, noise_sd) {
  #'
  assertthat::assert_that(
    d >= 2,
    msg="Treatment effect of complex data generating process can only be
     computed when the number of feature dimension is 2 or higher."
  )
  
  treatment_propensity <- 0.5
  
  X <- matrix(runif(n * d), nrow=n, ncol=d)
  W <- rbinom(n, size=1, prob=treatment_propensity)
  W <- matrix(W, ncol=1)
  
  treatment_effect <- treatment_effect_complex(X)
  
  main_effect <- rep(0, n)
  noise <- rnorm(n, sd=noise_sd)
  
  Y <- outcome(main_effect, W, treatment_effect, noise)
  Y <- matrix(Y, ncol=1)
  
  data <- list(X=X, W=W, Y=Y, treatment_effect=treatment_effect)
  return(data)
}


<<<<<<< HEAD:src/simulation/dgp.R
dgp_unbalanced <- function(n, d, noise_sd) {
  #'
  assertthat::assert_that(
    d >= 2,
    msg="Treatment effect of unbalanced data generating process can only be
     computed when the number of feature dimension is 2 or higher."
  )
  
  treatment_propensity <- 0.2
=======
dgp_boundary <- function(n, d, noise_sd) {
  #'
  assertthat::assert_that(
    d >= 5,
    msg="Main effect of boundary data generating process can only be computed
     when the number of feature dimension is 5 or higher."
  )
  
  treatment_propensity <- 0.5
>>>>>>> a81bf9372f00c83572e6603694ed01578d028440:src/dgp.R
  
  X <- matrix(rnorm(n * d), nrow=n, ncol=d)
  W <- rbinom(n, size=1, prob=treatment_propensity)
  W <- matrix(W, ncol=1)
  
  treatment_effect <- treatment_effect_unbalanced(X)
  main_effect <- main_effect_unbalanced(X, d)
  
  noise <- rnorm(n, sd=noise_sd)
  
  Y <- outcome(main_effect, W, treatment_effect, noise)
  Y <- matrix(Y, ncol=1)
  
  data <- list(X=X, W=W, Y=Y, treatment_effect=treatment_effect)
  return(data)
}


<<<<<<< HEAD:src/simulation/dgp.R
dgp_simple <- function(n, d, noise_sd) {
  #'
  treatment_propensity <- 0.5
=======
dgp_unbalanced <- function(n, d, noise_sd) {
  #'
  assertthat::assert_that(
    d >= 2,
    msg="Treatment effect of unbalanced data generating process can only be
     computed when the number of feature dimension is 2 or higher."
  )
  
  treatment_propensity <- 0.2
>>>>>>> a81bf9372f00c83572e6603694ed01578d028440:src/dgp.R
  
  X <- matrix(runif(n * d), nrow=n, ncol=d)
  W <- rbinom(n, size=1, prob=treatment_propensity)
  W <- matrix(W, ncol=1)
  
  treatment_effect <- treatment_effect_simple(X)
  
  main_effect <- main_effect_simple(X)
  noise <- rnorm(n, sd=noise_sd)
  
  Y <- outcome(main_effect, W, treatment_effect, noise)
  Y <- matrix(Y, ncol=1)
  
  data <- list(X=X, W=W, Y=Y, treatment_effect=treatment_effect)
  return(data)
}


<<<<<<< HEAD:src/simulation/dgp.R
dgp_boundary <- function(n, d, noise_sd, testing=FALSE) {
  #' Classical regression context without treatment effects.
  #'
=======
dgp_simple <- function(n, d, noise_sd) {
  #'
  treatment_propensity <- 0.5
  
  X <- matrix(runif(n * d), nrow=n, ncol=d)
  W <- rbinom(n, size=1, prob=treatment_propensity)
  W <- matrix(W, ncol=1)
>>>>>>> a81bf9372f00c83572e6603694ed01578d028440:src/dgp.R
  
  if (testing) {
    X <- draw_from_ring(n, d)
  } else {
    X <- matrix(rnorm(n * d), nrow=n, ncol=d)
  }
  
  main_effect <- main_effect_boundary(X)
  noise <- rnorm(n, sd=noise_sd)
  
  Y <- main_effect + noise
  Y <- matrix(Y, ncol=1)
  
  data <- list(X=X, Y=Y)
  return(data)
}
#------------------------- Helper functions -----------------------------------

outcome <- function(main_effect, W, treatment_effect, noise) {
  Y <- main_effect + (W - 0.5) * treatment_effect + noise
  return(Y)
}


zeta <- function(x, a=2, b=20, c=(1/3)) {
  return(a / (1 + exp(-b * (x - c))))
}


treatment_effect_complex <- function(X) {
  #'
  #' Dimensionality of X >= 2.
  #' 
  return(zeta(X[, 1]) * zeta(X[, 2]))
}


main_effect_unbalanced <- function(X, d) {
  #'
  beta <- runif(d, -5, 5)
  main_effect <- X %*% beta + 5 * 1*(X[, 1] > 0.5) + 4 * 1*(X[, 2] > 0.1)
  return(main_effect)
}


treatment_effect_unbalanced <- function(X) {
  #'
  #' Dimensionality of X >= 2.
  #' 
  treatment_effect <- 8 * 1*(X[, 2] > 0.1)
  return(treatment_effect)
}


<<<<<<< HEAD:src/simulation/dgp.R
treatment_effect_simple <- function(X) {
  #' 
  #' Unrestricted dimensionality of X.
  #' 
  treatment_effect <- rep(2, nrow(X))
=======
treatment_effect_boundary <- function(X) {
  #'
  #' Dimensionality of X >= 2.
  #' 
  treatment_effect <- X[, 1] + log(1 + exp(X[, 2]))
>>>>>>> a81bf9372f00c83572e6603694ed01578d028440:src/dgp.R
  return(treatment_effect)
}


<<<<<<< HEAD:src/simulation/dgp.R
main_effect_simple <- function(X) {
  #' Roos & Arnold (1963) - Function
  #' 
  XX <- abs(4 * X - 2)
  main_effect <- apply(XX, 1, prod)
=======
main_effect_boundary <- function(X) {
  #'
  main_effect <- max(X[, 1] + X[, 2], X[, 3], 0) + max(X[, 4], X[, 5], 0)
>>>>>>> a81bf9372f00c83572e6603694ed01578d028440:src/dgp.R
  return(main_effect)
}


<<<<<<< HEAD:src/simulation/dgp.R
main_effect_boundary <- function(X) {
  #'
  #' Dimensionality of X >= 2.
  #' 
  treatment_effect <- X[, 1] + log(1 + exp(X[, 2]))
=======
treatment_effect_simple <- function(X) {
  #' 
  #' Unrestricted dimensionality of X.
  #' 
  treatment_effect <- rep(2, nrow(X))
>>>>>>> a81bf9372f00c83572e6603694ed01578d028440:src/dgp.R
  return(treatment_effect)
}

#------------------------- Helper functions -----------------------------------

<<<<<<< HEAD:src/simulation/dgp.R
draw_from_ring <- function(n, d) {
  X_ <- matrix(rnorm(n * (d-2)), nrow=n, ncol=d-2)
  X <- matrix(nrow=0, ncol=2)
 
  while (nrow(X) < n) {
    x <- matrix(rnorm(2), nrow=1, ncol=2)
    if (sqrt(sum(x ** 2)) > 2.75) X <- rbind(X, x)
  }
  
  X <- cbind(X, X_)
  rownames(X) <- NULL
  return(X)
=======
main_effect_simple <- function(X) {
  #' Roos & Arnold (1963) - Function
  #' 
  XX <- abs(4 * X - 2)
  main_effect <- apply(XX, 1, prod)
  return(main_effect)
>>>>>>> a81bf9372f00c83572e6603694ed01578d028440:src/dgp.R
}
