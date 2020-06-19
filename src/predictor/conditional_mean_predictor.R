#' Functionalities to wrap existing packages to get a unified interface for the
#' estimation of the conditional mean.
#'
#' @example 
#' data_train <- dgp(...)
#' data_test <- dgp(...)
#' predictor <- conditional_mean_predictor("method", data_train, args)
#' te_estimate <- predict_conditional_mean("method", predictor, data_test)


suppressMessages(library("assertthat"))
suppressMessages(library("magrittr"))
suppressMessages(library("grf"))
suppressMessages(library("FNN"))
suppressMessages(library("randomForest"))


predict_conditional_mean <- function(
  method, predictor, data=NULL, X=data$X, Y=data$Y
) {
  #' Predict conditional mean function.
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
  treatment_effect <- switch(
    method,
    "rf" = estimate_mean_rf(predictor, data, X, Y),
    "grf" = estimate_mean_grf(predictor, data, X, Y),
    "llf" = estimate_mean_llf(predictor, data, X, Y),
    "knn" = estimate_mean_knn(predictor, data, X, Y),
    "ols" = estimate_mean_ols(predictor, data, X, Y),
    stop(sprintf("%s is not in set of implemented methods: {'grf', 'llf', 
                 'knn', 'ols'}", method))
  )
  return(treatment_effect)
}


conditional_mean_predictor <- function(
  method,
  data=NULL,
  X=data$X,
  Y=data$Y,
  n_trees=500,
  k=5,
  p=5,
  interaction=FALSE,
  n_threads=1,
  seed=0
) {
  #' Return predictor for conditional mean.
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
  predictor <- switch(
    method,
    "rf" = predictor_mean_rf(data, X, Y, n_trees, n_threads),
    "grf" = predictor_mean_grf(data, X, Y, n_trees, n_threads, seed),
    "llf" = predictor_mean_llf(data, X, Y, n_trees, n_threads),
    "knn" = predictor_mean_knn(data, X, Y, k),
    "ols" = predictor_mean_ols(data, X, Y, p, interaction),
    stop(sprintf("%s is not in set of implemented methods: {'grf', 'llf', 'knn',
                 'ols'}", method))
  )
  return(predictor)
}


predictor_mean_rf <- function(
  data=NULL, X=data$X, Y=data$Y, n_trees=1000, n_threads=1
) {
  #' Return conditional mean predictor using random forest.
  #' 
  rf <- randomForest::randomForest(
    x=X, y=as.numeric(Y), ntree=n_trees, random_state=0
    )
  return(rf)
}


estimate_mean_rf <- function(
  predictor, data=NULL, X=data$X, Y=data$Y
) {
  #' Estimate conditional mean using fitted random forest.
  #' 
  predictions <- as.numeric(predict(predictor, newdata=X))
  return(predictions)
}


predictor_mean_grf <- function(
  data=NULL, X=data$X, Y=data$Y, n_trees=1000, n_threads=1, seed=0
) {
  #' Return predictor of conditional mean using the generalized random forest.
  #' 
  rforest <- grf::regression_forest(
    X=X, Y=Y, num.trees=n_trees, tune.parameters="all", honesty=TRUE,
    num.threads=n_threads, seed=seed
  )
  return(rforest)
}


estimate_mean_grf <- function(
  predictor, data=NULL, X=data$X, Y=data$Y
) {
  #' Estimate conditional mean using fitted generalized random forest predictor.
  #' 
  predictions <- predict(predictor, X)[["predictions"]]
  return(predictions)
}


predictor_mean_knn <- function(
  data=NULL, X=data$X, Y=data$Y, k=5
) {
  #' Return predictor of k-nearest-neighbors algorithm.
  #' 
  predictor <- list(
    data_train=list(X=X, Y=Y),
    k=k
  )
  return(predictor)
}


estimate_mean_knn <- function(
  predictor, data=NULL, X=data$X, Y=data$Y
) {
  #' Estimate conditional mean using the training data.
  #'
  data_train <- predictor[["data_train"]] 
  k <- predictor[["k"]]
  
  X_train <- data_train[["X"]]
  Y_train <- data_train[["Y"]]
  
  nearest_neighbors <- FNN::get.knnx(X_train, query=X, k=k)
  nearest_neighbors_df <- data.frame(nearest_neighbors[["nn.index"]])
  
  outcomes <- sapply(
    nearest_neighbors_df, function(index_vec) Y_train[index_vec]
  )
  
  predictions <- rowMeans(outcomes)
  return(predictions)
}



predictor_mean_ols <- function(
  data=NULL, X=data$X, Y=data$Y, p=NULL, interaction=FALSE
) {
  #' Return predictor of linear (p-th degree pol.) model
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
  
  linear_predictor <- list(model=lm(Y ~ XX), p=p, interaction=interaction)
  return(linear_predictor)
}


estimate_mean_ols <- function(
  predictor, data=NULL, X=data$X, Y=data$Y
) {
  #' Estimate conditional mean with new data on a linear predictor
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
  
  model <- predictor[["model"]]
  coef_ <- coef(model)
  
  predictions <- coef_[1] + XX %*% coef_[-1]
  return(predictions)
}


predictor_mean_llf <- function(
  data=NULL, X=data$X, Y=data$Y, n_trees=1000, n_threads=1, seed=0
) {
  #' Return predictor of conditional mean using the local linear forest.
  #' 
  llf <- grf::ll_regression_forest(
    X, Y, enable.ll.split=TRUE, num.trees=n_trees, num.threads=n_threads, seed=0
    )
  tuned_lambda <- grf::tune_ll_regression_forest(llf)
  return(list("llf"=llf, "lambda"=tuned_lambda[["lambda.min"]]))
}


estimate_mean_llf <- function(
  predictor, data=NULL, X=data$X, Y=data$Y
) {
  #' Estimate conditional mean using fitted local linear forest predictor.
  #' 
  llf <- predictor[["llf"]]
  lambda <- predictor[["lambda"]]
  
  predictions <- predict(llf, newdata=X, ll.lambda=lambda)[["predictions"]]
  return(predictions)
}
