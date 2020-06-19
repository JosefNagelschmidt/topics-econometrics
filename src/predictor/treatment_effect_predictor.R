#' Functionalities to wrap existing packages to get a unified interface for the
#' estimation of treatment effects.
#'
#' @example 
#' data_train <- dgp(...)
#' data_test <- dgp(...)
#' predictor <- treatment_effect_predictor("method", data_train, args)
#' te_estimate <- predict_treatment_effect("method", predictor, data_test)


suppressMessages(library("assertthat"))
suppressMessages(library("magrittr"))
suppressMessages(library("grf"))
suppressMessages(library("FNN"))
suppressMessages(library("causalToolbox"))


predict_treatment_effect <- function(
  method, predictor, data=NULL, X=data$X, W=data$W, Y=data$Y
) {
  #' Predict (heterogeneous) treatment effects on new data given a predictor.
  #' 
  #' @description Wrapper function for the prediction of heterogeneous treatment
  #' effects on new data.
  #'
  #' @param method: Method used for the estimation of the treatment effects.
  #' Currently supported methods {'grf', 'knn', 'trf', 'const'}.
  #' @param predictor: A fitted predictor object returned by function
  #' ``treatment_effect_predictor``.
  #' @param data: List containing data X, W, Y as described below. Default is
  #' NULL.
  #' @param X: Data matrix or frame, of shape (n x p).
  #' @param W: Binary treatment indicator vector, of length n.
  #' @param Y: Outcome vector, of length n.
  #' @param seed: Integer representing the random number generator seed. Default
  #' is 0.
  #' 
  #' @return treatment_effect: The predicted treatment effects on the newly
  #' observed data given the fitted predictor.
  #' 
  #' @examples
  #' X <- rnorm(10)
  #' W <- rbinom(10, 1, 0.5)
  #' Y <- X + 2 * (W - 0.5)
  #' predictor <- treatment_effect_predictor("const", X=X, W=W, Y=Y)
  #' te_estimate <- predict_treatment_effect("const", predictor, X=X, W=W, Y=Y)
  #' 
  #' data_train <- dgp("complex_treatment", n=1000, d=10)
  #' data_test <- dgp("complex_treatment", n=100, d=10)
  #' 
  #' predictor <- treatment_effect_predictor("grf", data_train)
  #' te_estimate <- predict_treatment_effect("grf", predictor, data_test)
  
  treatment_effect <- switch(method,
    "grf" = estimate_grf(predictor, data, X, Y, W),
    "trf" = estimate_trf(predictor, data, X, Y, W),
    "const" = estimate_const(predictor, data, X, Y, W),
    "knn" = estimate_knn(predictor, data, X, Y, W),
    "ols" = estimate_ols(predictor, data, X, Y, W),
    stop(sprintf("%s is not in set of implemented methods: {'grf', 'trf', 
                 'const', 'knn', 'ols'}", method))
  )
  return(treatment_effect)
}


treatment_effect_predictor <- function(
  method,
  data=NULL,
  X=data$X,
  W=data$W,
  Y=data$Y,
  n_trees=500,
  k=NULL,
  p=5,
  interaction=FALSE,
  n_threads=1,
  seed=0
) {
  #' Estimate (heterogeneous) treatment effect predictor.
  #' 
  #' @description Wrapper function for the estimation of heterogeneous treatment
  #' effects.
  #'
  #' @param method: Method used for the estimation of the treatment effects.
  #' Currently supported methods {'grf', 'knn', 'trf', 'const'}.
  #' @param data: List containing data X, W, Y as described below. Default is
  #' NULL.
  #' @param X: Data matrix or frame, of shape (n x p).
  #' @param W: Binary treatment indicator vector, of length n.
  #' @param Y: Outcome vector, of length n.
  #' @param n_trees: Positive integer denoting the number of trees used for the
  #' fitting of the random forest, if method is "grf" or "trf".
  #' @param k: Positive integer denoting the number of neighbors to use for
  #' k-nearest-neighbor algorithm, if method is "knn".
  #' @param p: Positive integer denoting the number of polynomial terms used
  #' for the fitting of the least squares model, if method is "ols".
  #' @param n_threads: Positive integer denoting the number of threads to use
  #' for parallelization.
  #' @param seed: Integer representing the random number generator seed. Default
  #' is 0.
  #' 
  #' @return predictor: An object that can be used to predict treatment effects
  #' on new data.
  #' 
  #' @examples
  #' X <- rnorm(10)
  #' W <- rbinom(10, 1, 0.5)
  #' Y <- X + 2 * (W - 0.5)
  #' predictor <- treatment_effect_predictor("const", X=X, W=W, Y=Y)
  #' 
  #' data <- dgp("complex_treatment", n=1000, d=10)
  #' predictor <- treatment_effect_predictor("grf", data)
  predictor <- switch(method,
    "grf" = predictor_grf(data, X, W, Y, n_trees, n_threads, seed),
    "trf" = predictor_trf(data, X, W, Y, n_trees, n_threads),
    "knn" = predictor_knn(data, X, W, Y, k),
    "const" = predictor_const(data, X, W, Y),
    "ols" = predictor_ols(data, X, W, Y, p, interaction),
    stop(sprintf("%s is not in set of implemented methods: {'grf', 'trf', 
                 'knn', 'const', 'ols'}", method))
  )
  return(predictor)
}


predictor_trf <- function(
  data=NULL, X=data$X, W=data$W, Y=data$Y, n_trees=1000, n_threads=1
) {
  #' Return treatment effect predictor using T-learner with random forest base.
  #' 
  forestry_args <- list(
    relevant.Variable=1:ncol(X), ntree=n_trees, replace=TRUE,
    sample.fraction=0.9, mtry=ncol(X), nodesizeSpl=1, nodesizeAvg=3,
    splitratio=0.5, middleSplit=FALSE
    )
  
  w <- as.vector(W)
  y <- as.vector(Y)
  
  trf <- causalToolbox::T_RF(
    X, w, y,
    nthread=n_threads, mu0.forestry=forestry_args, mu1.forestry=forestry_args
    )
  
  return(trf)
}


estimate_trf <- function(
  predictor, data=NULL, X=data$X, W=data$W, Y=data$Y, n_trees=1000, n_threads=1
) {
  #' Estimate treatment effect using fitted T-learner with random forest base.
  #' 
  treatment_effect <- causalToolbox::EstimateCate(predictor, X)
  return(treatment_effect)
}


predictor_const <- function(data=NULL, X=data$X, W=data$W, Y=data$Y) {
  #' Return predictor for treatment effect using simple mean differences.
  #' 
  W <- as.logical(W)
  predictor <- mean(Y[W]) - mean(Y[!W])
  return(predictor)
}


estimate_const <- function(
  predictor, data=NULL, X=data$X, W=data$W, Y=data$Y
) {
  #' Estimate treatment effect using predictor of simple mean differences.
  #' 
  treatment_effect <- rep(predictor, nrow(X))
  return(treatment_effect)
}


predictor_grf <- function(
  data=NULL, X=data$X, W=data$W, Y=data$Y, n_trees=1000, n_threads=1, seed=0
  ) {
  #' Return predictor of treatment effect using the generalized random forest.
  #' 
  cforest = grf::causal_forest(
    X=X, Y=Y, W=W, num.trees=n_trees, tune.parameters="all", honesty=TRUE,
    num.threads=n_threads, seed=seed
    )
  return(cforest)
}


estimate_grf <- function(
  predictor, data=NULL, X=data$X, W=data$W, Y=data$Y
  ) {
  #' Estimate treatment effect using fitted generalized random forest predictor.
  #' 
  treatment_effect <- predict(predictor, X)$predictions
  return(treatment_effect)
}


predictor_knn <- function(
  data=NULL, X=data$X, W=data$W, Y=data$Y, k=5
) {
  #' Return predictor of k-nearest-neighbors algorithm
  #' 
  predictor <- list(
    data_train=list(X=X, W=W, Y=Y),
    k=k
  )
  return(predictor)
}


estimate_knn <- function(
  predictor, data=NULL, X=data$X, W=data$W, Y=data$Y
) {
  #' Estimate treatment effect using the training data.
  #'
  data_train <- predictor[["data_train"]] 
  k <- predictor[["k"]]
  
  X_train <- data_train[["X"]]
  W_train <- data_train[["W"]]
  Y_train <- data_train[["Y"]]
  
  w <- as.logical(W_train)
  
  X_treated <- X_train[w, ]
  X_untreated <- X_train[!w, ]
  
  treated_nn <- FNN::get.knnx(X_treated, query=X, k=k)
  untreated_nn <- FNN::get.knnx(X_untreated, query=X, k=k)
  
  treated_nn_df <- data.frame(treated_nn[["nn.index"]])
  untreated_nn_df <- data.frame(untreated_nn[["nn.index"]])
  
  potential_outcome_treated <- sapply(
    treated_nn_df, function(index_vec) Y_train[w][index_vec]
  )
  
  potential_outcome_untreated <- sapply(
    untreated_nn_df, function(index_vec) Y_train[!w][index_vec]
  )
  
  potential_outcome_treated <- rowMeans(potential_outcome_treated)
  potential_outcome_untreated <- rowMeans(potential_outcome_untreated)
  
  treatment_effect <- potential_outcome_treated - potential_outcome_untreated
  return(treatment_effect)
}



predictor_ols <- function(
  data=NULL, X=data$X, W=data$W, Y=data$Y, p=NULL, interaction=FALSE
) {
  #' Return predictor of linear (10th degree pol.) model
  #' 
  if (p %>% is.null) p <- 1
  
  if (interaction) {
    XX <- model.matrix( ~ poly(X, degree=p, raw=TRUE))[, -1]
  } else {
    XX <- X
    if (p > 1) {
      for (i in 2:p) XX <- cbind(XX, X**i)
    }
  }
  
  X_center <- scale(XX, scale=FALSE)
  W_X_center <- sweep(X_center, 1, W, "*")
  
  features <- cbind(W, XX, W_X_center)
  
  linear_predictor <- list(model=lm(Y ~ features), p=p, interaction=interaction)
  return(linear_predictor)
}


estimate_ols <- function(
  predictor, data=NULL, X=data$X, W=data$W, Y=data$Y
) {
  #' Estimate treatment effect with new data on a linear predictor
  #'
  p <- predictor[["p"]]
  n <- nrow(X)
  
  if (predictor[["interaction"]]) {
    XX <- model.matrix( ~ poly(X, degree=p, raw=TRUE))[, -1]
  } else {
    XX <- X
    if (p > 1) {
      for (i in 2:p) XX <- cbind(XX, X**i)
    }
  }
  
  X_center <- scale(XX, scale=FALSE)
  
  ones <- rep(1, n)
  
  features_untreated <- cbind(0 * ones, XX, 0 * X_center)
  features_treated <- cbind(ones, XX, X_center)
  
  model <- predictor[["model"]]
  coef_ <- coef(model)
  
  potential_outcome_treated <- coef_[1] + features_treated %*% coef_[-1]
  potential_outcome_untreated <- coef_[1] + features_untreated %*% coef_[-1]
  
  treatment_effect <- potential_outcome_treated - potential_outcome_untreated
  return(treatment_effect)
}
