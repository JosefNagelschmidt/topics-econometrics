rm(list = ls())
library("grf")
library("glmnet")
library("caret")
library("dplyr")   


# two-fold cross-validation method for ridge lambda (should be improved, preliminary):
choose_lambda = function(X, Y, lambdas, correction){
    X_first <- head(X,dim(as.matrix(X))[1]/2)
    Y_first <- head(Y, as.numeric(length(Y)/2))
    X_sec   <- tail(X,dim(as.matrix(X))[1]/2)
    Y_sec   <- tail(Y, as.numeric(length(Y)/2))
    
    test_forest = ll_regression_forest(X_first, Y_first, enable.ll.split = TRUE, ll.split.lambda = 0.1)
    test_CART = ll_regression_forest(X_first, Y_first, enable.ll.split = FALSE)
  
  errors = sapply(lambdas, function(lambda){
    preds.llf = predict(test_forest, X_sec, linear.correction.variables = correction, ll.lambda = lambda, ll.weight.penalty = FALSE)$predictions 
    mean( (Y_sec - preds.llf)**2 )
  })
  
  errors_CART = sapply(lambdas, function(lambda){
    preds.llf.CART = predict(test_CART, X_sec, linear.correction.variables = correction, ll.lambda = lambda, ll.weight.penalty = FALSE)$predictions 
    mean( (Y_sec - preds.llf.CART)**2 )
    })
  
  return(list(lambda_LLF= lambdas[which.min(errors)], lambda_CART = lambdas[which.min(errors_CART)]))
}

# first DGP (cond. exp. function):
simulation <- function(n, version){
  
  if(version == 1){
    X <- matrix(runif(n*version, min = 0, max = 1), nrow = n, ncol = version)
    Y <- 20* (X[,1] - 0.5)**3 
  }
  
  for(i in 2:3){
    if(version == i){
      X <- matrix(runif(n*version, min = 0, max = 1), nrow = n, ncol = version)
      Y <- 20* (X[,1] - 0.5)**3 + 10*X[,2] + 10* ifelse(version>2,X[,3],0)
    }
  }
  
  for(i in 4:5){
    if(version == i){
       X <- matrix(runif(n*version, min = 0, max = 1), nrow = n, ncol = version)
       Y <- 20* (X[,1] - 0.5)**3 + 10*X[,2] + 10*X[,3] + 5*X[,4] + 5 * ifelse(version>4,X[,5],0)
    }
  }
  
  for(i in 6:20){
    if(version == i){
      X <- matrix(runif(n*version, min = 0, max = 1), nrow = n, ncol = version)
      Y <- 20* (X[,1] - 0.5)**3 + 10*X[,2] + 10*X[,3] + 5*X[,4] + 5*X[,5]
      for(j in 6:i){
        Y = Y + 2*X[,j]
      }
    }
  }
  output <- list(X= X, Y = Y)
  return(output)
}

# second DGP (conditional exp. function):
friedman_sim <- function(n,lower,upper){
  X <- matrix(runif(n*5, min = lower, max = upper), nrow = n, ncol = 5)
  Y <- 10*sin(pi*X[,1]*X[,2]) + 20 * (X[,3]-0.5)**2 +10* X[,4] + 5* X[,5] + 1*rnorm(n=n)
  output <- list(X= X, Y = Y)
  return(output)
}

# grid of potential lambdas
lambdas = c(0,0.001,0.01, 0.1, 0.3, 0.6, 0.8, 1)

num.reps = 5  # number of replications per setup

ns = seq(from=1, to=10, by=1) # looping over variable number of regressors for first DGP
#ns = c(500,1000)  # for friedman simulation selection sample sizes

results = sapply(ns, function(version){
  basic.results = replicate(num.reps, {
    
    # first DGP
    dat = simulation(n = 600, version = version)  # here version selects the number of covariates we regress on
    dat.test = simulation(n = 600, version = version)
    
    # second DGP
    #dat = friedman_sim(n= version,lower=0, upper=1)  # here version selects the sample size 
    #dat.test = friedman_sim(n = version, lower=0, upper=1)
    
    # GRF
    forest <- regression_forest(as.matrix(dat$X), as.numeric(dat$Y))
    preds <- predict(forest, as.matrix(dat.test$X))$predictions
    grf.err = mean((preds - dat.test$Y)**2)
    
    # LLF- CART split:
    # selecting the variables/regressors to correct for/penalize for during ridge regression
    # this correction is for the first DGP
    correction = seq(from=1, to=version, by=1)
    # this one is for the second DGP
    #correction = seq(from=1, to=5, by=1)
   
    forestLLF_CART <- ll_regression_forest(as.matrix(dat$X), as.numeric(dat$Y), enable.ll.split = FALSE)
    # cross-validation choosing lambda for ridge
    lambda_list = choose_lambda(dat$X, dat$Y, lambdas, correction=correction)
    print(lambda_list)
    
    preds.llf_CART = predict(forestLLF_CART, as.matrix(dat.test$X), linear.correction.variables = correction, ll.lambda = lambda_list[[2]])$predictions
    err.llf_CART = mean((preds.llf_CART - dat.test$Y)**2)
    
     # LLF - ridge split : 
    # training the forest
    forestLLF <- ll_regression_forest(as.matrix(dat$X), as.numeric(dat$Y), enable.ll.split = TRUE, ll.split.lambda = 0.1)
    preds.llf = predict(forestLLF, as.matrix(dat.test$X), linear.correction.variables = correction, ll.lambda = lambda_list[[1]])$predictions
    err.llf = mean((preds.llf - dat.test$Y)**2)
    
    print(c(sqrt(grf.err),sqrt(err.llf),sqrt(err.llf_CART)))
    return(c(sqrt(grf.err),sqrt(err.llf),sqrt(err.llf_CART)))
  })
  basic.results = data.frame(t(basic.results))
  colMeans(basic.results)
})
results = data.frame(t(results))

