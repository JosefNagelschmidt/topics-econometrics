rm(list = ls())

library("grf")
library("glmnet")
library("forestry")
library("causalToolbox")

lasso <- function(A,Y){
  lasso.mod = cv.glmnet(as.matrix(A), as.numeric(Y), alpha = 1)
  selected = which(coef(lasso.mod) != 0)
  if(length(selected) < 2){
    selected = 1:ncol(A)
  }else{ 
    selected = selected[-1] - 1 # remove intercept
  }
  return(selected)
}

xllforests <- function(dat, dat.test){
  p = dim(dat$X)[2]
  # bring data in the correct form
  Y_0 = dat$Y[cbind(dat$Y, dat$W)[ , 2] == 0]
  Y_1 = dat$Y[cbind(dat$Y, dat$W)[ , 2] == 1]
  X.expand <- cbind(dat$X,dat$W)
  X_0 = dat$X[X.expand[ ,p+1] == 0, ]
  X_1 = dat$X[X.expand[ ,p+1] == 1, ]
  
  ##### Linear base learner 1 Stage #############
  m.0.hatLLF <- ll_regression_forest(X_0, Y_0, tune.parameters = TRUE)
  m.1.hatLLF <- ll_regression_forest(X_1 , Y_1, tune.parameters = TRUE)
  tuned.lambda_m0 = tune_ll_regression_forest(m.0.hatLLF)
  tuned.lambda_m1 = tune_ll_regression_forest(m.1.hatLLF)
  
  
  ### find out which linear correction variables for step 1 
  selected_0 = lasso(A= X_0, Y=Y_0)
  selected_1 = lasso(A=X_1, Y=Y_1)
  
  predictions.m0 = predict(m.0.hatLLF, X_1, linear.correction.variables = selected_0, lambda = tuned.lambda_m0)
  predictions.m1 = predict(m.1.hatLLF, X_0, linear.correction.variables = selected_1, lambda = tuned.lambda_m1)
  
  ## 2 step
  D_1 <- Y_1 - predictions.m0$predictions
  D_0 <- predictions.m1$predictions - Y_0
  
  tau.hat.1 <- ll_regression_forest(X_1, D_1, tune.parameters = TRUE)
  tau.hat.0 <- ll_regression_forest(X_0, D_0, tune.parameters = TRUE)
  
  selected_d0 = lasso(A= X_0, Y= D_0)
  selected_d1 = lasso(A= X_1, Y= D_1)
  
  ## propensity estimates
  prop.hat <- ll_regression_forest(dat$X, as.numeric(dat$W) , tune.parameters = TRUE)
  tuned.lambda_prop = tune_ll_regression_forest(prop.hat)
  selected_w = lasso(A= dat$X, Y= dat$W)
  
  g_weights <- predict(prop.hat, dat.test$X, linear.correction.variables = selected_w, lambda = tuned.lambda_prop)
  
  ## test data
  tuned.lambda_t.hat1 = tune_ll_regression_forest(tau.hat.1)
  pred_tau.hat_1 <- predict(tau.hat.1, dat.test$X, linear.correction.variables = selected_d1, lambda = tuned.lambda_t.hat1)
  tuned.lambda_t.hat0 = tune_ll_regression_forest(tau.hat.0)
  pred_tau.hat_0 <- predict(tau.hat.0, dat.test$X, linear.correction.variables = selected_d0, lambda = tuned.lambda_t.hat0)
  
  pred.final <- g_weights$predictions * pred_tau.hat_0$predictions + (1 - g_weights$predictions) * pred_tau.hat_1$predictions
  return(pred.final)
}

simulation <- function(n = 5000, d = 2, version = 1){
  sigma <- 1  # define noise level (cases in paper: 1 and 2)
  X <- matrix(runif(n*d, min = 0, max = 1), nrow = n, ncol = d)  # d defines the dimension of the covariate space, n sample size
  noise <- sigma*rnorm(n=n)
  if(version == 1 | version == 2 | version == 3){
    main_effect <- rep(0, times = n)
    treatment_propensity <- 0.5
    W <- matrix(rbinom(n = n,
                       size = 1,
                       prob = treatment_propensity),
                nrow = n, ncol = 1)
    if(version == 1 | version == 2){ # setup 3, "complex treatment" (version 1)
      zeta1 <- 2/(1+exp(-20*(X[,1]-(1/3))))
      zeta2 <- 2/(1+exp(-20*(X[,2]-(1/3))))
    }
    if(version ==3){
      zeta1 <- 1 + 1/(1+exp(-20*(X[,1]-(1/3))))
      zeta2 <- 1 + 1/(1+exp(-20*(X[,2]-(1/3))))
    }
    Tau <- matrix(zeta1 * zeta2,nrow=n, ncol=1)
    if(version == 1 | version == 3){ # setup 3, "complex treatment"
      Y <- matrix(main_effect + (W-0.5) * Tau + noise, nrow=n, ncol=1)
    }
    if(version == 2){
      Y <- matrix(main_effect + W * Tau + noise, nrow=n, ncol=1)  ## here we see that minor changes affect the performance greatly
    }    # because the glmet lasso cannot find the relevant covariates in order to correct for those in the LLF
  }
  if(version == 4){ ## see Wager and Nie setup A
    main_effect <- sin(pi * X[,1] * X[,2]) + 2*(X[,3]-0.5)^2 + X[,4] + 0.5*X[,5]
    treatment_propensity <- max(0.1, min(sin(pi * X[,1] * X[,2]),0.9))
    W <- matrix(rbinom(n = n,
                       size = 1,
                       prob = treatment_propensity),
                nrow = n, ncol = 1)
    Tau <- matrix(0.5*(X[,1] * X[,2]),nrow=n, ncol=1)
    Y <- matrix(main_effect + (W-0.5) * Tau + noise, nrow=n, ncol=1)
  }
  if(version ==5){ # setup 2, "boundary setting"
    X <- matrix(rnorm(n*d), nrow = n, ncol = d)
    main_effect <- max(X[,1]+ X[,2],X[,3],0) + max(X[,4]+X[,5],0)
    treatment_propensity <- 0.5
    W <- matrix(rbinom(n = n,
                       size = 1,
                       prob = treatment_propensity),
                nrow = n, ncol = 1)
    Tau <- matrix(X[,1] + log(1+exp(X[,2])),nrow=n, ncol=1)
    Y <- matrix(main_effect + (W-0.5) * Tau + noise, nrow=n, ncol=1)
  }
  if(version ==6){ # setup 4, "confounding"
    X <- matrix(rnorm(n*d), nrow = n, ncol = d)
    main_effect <- 2*log(1+exp(X[,1]+X[,2]+X[,3]))
    treatment_propensity <- 1/(1+exp(X[,2])+exp(X[,3]))
    W <- matrix(rbinom(n = n,
                       size = 1,
                       prob = treatment_propensity),
                nrow = n, ncol = 1)
    Tau <- matrix(1,nrow=n, ncol=1)
    Y <- matrix(main_effect + (W-0.5) * Tau + noise, nrow=n, ncol=1)
  }
  if(version ==7){ # setup 1, "unbalanced design"
    X <- matrix(rnorm(n*d), nrow = n, ncol = d)
    beta <- runif(d, min = -5, max = 5)
    main_effect <- X %*% beta + 5* ifelse(X[,1]>0.5,1,0) + 0.5 * 8 * ifelse(X[,2]>0.1,1,0)
    treatment_propensity <- 0.1
    W <- matrix(rbinom(n = n,
                       size = 1,
                       prob = treatment_propensity),
                nrow = n, ncol = 1)
    Tau <- matrix(8*ifelse(X[,2]>0.1,1,0) , nrow=n, ncol=1)
    Y <- matrix(main_effect + (W-0.5) * Tau + noise, nrow=n, ncol=1)
  }
  output <- list(X= X, W = W, Y = Y, Tau = Tau)
  return(output)
}


num.reps = 5  # number of replications per setup
lambdas = c(0, 0.1, 0.3, 0.5, 0.7, 1, 1.5)  # values from Friedberg et al. (2018)
ns = c(500,1000)  # sample sizes used in paper

choose_lambda = function(X, Y, W, selected, lambdas, forest){
  errors = sapply(lambdas, function(lambda){
    preds.llf = predict(forest, X, linear.correction.variables = selected, ll.lambda = lambda, ll.weight.penalty = TRUE)$predictions 
    mean( ((2*W - 1)*Y - preds.llf)**2 )
  })
  return(lambdas[which.min(errors)])
}

results = sapply(ns, function(n){
  basic.results = replicate(num.reps, {
    dat = simulation(n = n, d = 10, version = 7)
    dat.test = simulation(n = 2000, d = 10, version = 7)
    
    # X-BART
    xl_bart <- X_BART(feat = list(dat$X), tr = as.numeric(dat$W), yobs = as.numeric(dat$Y))
    cate_esti_bart = EstimateCate(xl_bart, dat.test$X)
    err_xbart = mean((cate_esti_bart - dat.test$Tau)**2)
  
    #T-RF 
    tl_rf <- T_RF(feat = list(dat$X), tr = as.numeric(dat$W), yobs = as.numeric(dat$Y))
    cate_tl_rf = EstimateCate(tl_rf, list(dat.test$X))
    err_trf = mean((cate_tl_rf - dat.test$Tau)**2)
    
   
    # X-RF
   # xl_rf <- X_RF(feat = list(dat$X), tr = as.numeric(dat$W), yobs = as.numeric(dat$Y))
   # cate_esti_rf = EstimateCate(xl_rf, list(dat.test$X))
   # err_xrf = mean((cate_esti_rf - dat.test$Tau)**2)
   
    # GRF 
    forest = causal_forest(as.matrix(dat$X), as.numeric(dat$Y), as.numeric(dat$W), tune.parameters = TRUE, num.trees = 2000) 
    preds = predict(forest, as.matrix(dat.test$X))$predictions
    grf.err = mean((preds - dat.test$Tau)**2)
    
    # LLCF
    lasso.mod = cv.glmnet(as.matrix(dat$X), as.numeric(dat$Y), alpha = 1)
    selected = which(coef(lasso.mod) != 0)
    if(length(selected) < 2){
      selected = 1:ncol(dat$X)
    }else{ 
      selected = selected[-1] - 1 # remove intercept
    }
  
    lambda = choose_lambda(as.matrix(dat$X), as.numeric(dat$Y), as.numeric(dat$W), selected, lambdas, forest)
    
    preds.llf = predict(forest, as.matrix(dat.test$X), linear.correction.variables = selected, 
                        ll.lambda = lambda, ll.weight.penalty = TRUE)$predictions
    err.llf = mean((preds.llf - dat.test$Tau)**2)
   
    ## X-LLF
    pred_xllf <- xllforests(dat = dat, dat.test = dat.test)
    err_xllf = mean((pred_xllf - dat.test$Tau)**2)
    
    
    print(c(sqrt(err_xbart),sqrt(err_trf),sqrt(grf.err),sqrt(err.llf), sqrt(err_xllf)))
    return(c(sqrt(err_xbart),sqrt(err_trf),sqrt(grf.err),sqrt(err.llf), sqrt(err_xllf)))
  })
  basic.results = data.frame(t(basic.results))
  colMeans(basic.results)
})
results = data.frame(t(results))
colnames(results) = c("X-BART","T-RF", "GRF", "LLF", "X-LLF")
results$n = ns 
results
write.csv(results,"LLF_replication.csv", row.names = FALSE)


