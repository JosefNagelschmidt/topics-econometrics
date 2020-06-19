# Exports main function ``monte_carlo_simulation`` which compares various
# methods to estimate heterogeneous treatment effects in multiple data settings.

source("src/simulation/dgp.R")
<<<<<<< HEAD:src/simulation/simulation.R
source("src/predictor/treatment_effect_predictor.R")
source("src/predictor/conditional_mean_predictor.R")
=======
source("src/predictor.R")
>>>>>>> a81bf9372f00c83572e6603694ed01578d028440:src/simulation.R
suppressMessages(library("furrr"))
suppressMessages(library("purrr"))
suppressMessages(library("tidyr"))
suppressMessages(library("dplyr"))
suppressMessages(library("tibble"))
suppressMessages(library("magrittr"))


monte_carlo_simulation <- function(
  n_sim,
  method_list,
  version_list,
  n_list,
  d_list,
  noise_sd_list=1,
  treatment=TRUE,
  seed_list=NULL,
  verbose=FALSE,
  progress_bar=TRUE,
  n_threads=2
) {
  #' Monte Carlo simulation.
  #'
  #' @description This function constructs a parameter grid from the input
  #' arguments and runs for each parameter constellation ``n_sim`` simulations.
  #' Aggregate measures "mean absolute error" and "root mean squared error" are
  #' returned for each constellation.
  #' 
  #' @param n_sim: Positive integer denoting the number of Monte Carlo draws.
  #' @param method_list: List or vector containing (str) names of which methods
  #' to use. Elements can be in {'grf', 'trf', 'naive', etc.}
  #' @param version_list: List or vector containing (str) names of which data
  #' processes to use. Elements can be in {'complex', 'unbalanced', 'boundary'}.
  #' @param n_list: List or vector containing (int) values denoting the number
  #' of samples to use for the data generation.
  #' @param d_list: List or vector containing (int) values denoting the number
  #' of features to use for the data generation.
  #' @param noise_sd_list: List or vector containing positive (float) values
  #' denoting the standard deviation of the error term in the data generation.
  #' @param treatment: Logical, if TRUE we do a simulation concerning the
  #' estimation of treatment effects, if FALSE, we consider regular conditional
  #' mean estimation.
  #' @param seed_list: List or vector containing (int) values denoting the
  #' seeds to set for each simulation. Default value is set to ``seq(n_sim)``.
  #' @param n_threads: Positive (int) denoting the number of threads to use
  #' for parallelization. Default is 1.
  #'
  
  # assert input
  assert_that(n_sim %>% is.count)
  if (seed_list %>% is.null) seed_list <- seq(n_sim)
  assert_that(length(seed_list) == n_sim)
  assert_that(verbose %>% is.logical)
  assert_that(progress_bar %>% is.logical)
  if (verbose %>% isTRUE) {
    cat("Note that option verbose is not working when parallelizing.") 
  }
  if ((verbose %>% isTRUE) & (progress_bar %>% isTRUE)) {
    progress_bar <- FALSE
    warning("Argument verbose and progress_bar cannot both be TRUE. Setting
             progress_bar to FALSE.", immediate.=TRUE)
  }
  
  # partialed function
  single_monte_carlo_run <- function(...) {
    #' Monte Carlo run for one parameter constellation wrapper.
    result <- single_monte_carlo_run_outer(seed_list, verbose, treatment, n_threads, ...)
    return(result)
  }
  
  grid <- construct_grid(
    method_list, version_list, n_list, d_list, noise_sd_list
    )
  
  result <- furrr::future_map(
    grid[["params"]],
    function(kwargs) do.call(single_monte_carlo_run, kwargs),
    .progress=progress_bar
    )
  
  df <- combine_results(result, grid[["df"]])
  return(df)
}


#-----------------------------Helper functions----------------------------------

single_monte_carlo_run_outer <- function(
  seed_list, verbose, treatment, n_threads, method, version, n, d, noise_sd
) {
  #' Single (outer) Monte Carlo run. Returns named list of evaluated metrics.
  metrics <- sapply(
    seed_list, 
    function(seed)
      single_monte_carlo_run_inner(
        method, version, n, d, seed, noise_sd, treatment, n_threads, verbose
        )
    )
  
  aggregated <- aggregate_metrics(metrics)
  return(aggregated)
}


single_monte_carlo_run_inner <- function(
  method, version, n, d, seed, noise_sd, treatment, n_threads, verbose=TRUE
) {
  #' Single Monte Carlo run. Returns named list of evaluated metrics.
  if (verbose %>% isTRUE) {
    where_are_we <- paste(
      "method: ", method, "; version: ", version, "; n: ", n, "; d: ", d,
      "; noise_sd ", noise_sd, "\n"
      )
    cat(where_are_we)
  }

  data_train <- dgp(version, n, d, seed, noise_sd)
<<<<<<< HEAD:src/simulation/simulation.R
  data_test <- dgp(version, n=1000, d, -1, noise_sd, testing=TRUE)
  
  if (treatment) {
    predictor_func <- treatment_effect_predictor
    estimate_func <- predict_treatment_effect
    quantity_of_interest <- "treatment_effect"
  } else {
    predictor_func <- conditional_mean_predictor
    estimate_func <- predict_conditional_mean
    quantity_of_interest <- "Y"
  }
  
  predictor <- predictor_func(
    method,
    data_train,
    n_trees=500,
    k=heuristic_k(n),
    p=heuristic_p(n, d),
    n_threads=n_threads
=======
  data_test <- dgp(version, n=500, d, -1, noise_sd)
  
  predictor <- treatment_effect_predictor(
    method,
    data_train,
    n_trees=500,
    k=floor(0.05*n),
    p=heuristic_p(n, d),
    n_threads=2
>>>>>>> a81bf9372f00c83572e6603694ed01578d028440:src/simulation.R
    )
  estimate <- estimate_func(method, predictor, data_test)
  
  metrics <- compute_metrics(data_test[[quantity_of_interest]], estimate)
  return(metrics)
}


construct_grid <- function(
  method_list, version_list, n_list, d_list, noise_sd_list
) {
  #' Construct parameter grid as data frame and list.
  #' 
  #' We remove boundary dgp with d < 5.
  #' 
  columns <- c("method", "version", "n", "d", "noise_sd")
  
  df_grid <- tidyr::expand_grid(
    method_list, version_list, n_list, d_list, noise_sd_list
  )
  colnames(df_grid) <- columns
  
  df_grid <- df_grid %>% filter(!((version=="boundary") & (d < 5)))
  df_grid <- df_grid %>% filter(!((version=="unbalanced") & (n < 250)))
  params_grid <- df_grid %>% split(seq(nrow(df_grid)))
  
  out <- list(params=params_grid, df=df_grid)
  return(out)
}


combine_results <- function(result, df) {
  #' Combine information from parameter grid and results.
  result <- tibble::as_tibble(do.call(rbind, result))
  df <- df %>% dplyr::bind_cols(result)
  return(df)
}


compute_metrics <- function(true, estimate) {
  #' Compute various metrics and return in named list.
  mae <- function(a, b) mean(abs(a - b))
  rmse <- function(a, b) sqrt(mean((a - b)**2))
  
  mae_ <- mae(true, estimate)
  rmse_ <- rmse(true, estimate)
  
  out <- c(mae=mae_, rmse=rmse_)
  return(out)
}


aggregate_metrics <- function(metrics) {
  #'
  means <- rowMeans(metrics)
  return(means)
}


heuristic_p <- function(n, d) {
  #'
  p <- floor(log(n / (2 * d), 1.75))
  if (p < 1) p <- 1
  if (p > 10) p <- 10
  return(p)
}
<<<<<<< HEAD:src/simulation/simulation.R

heuristic_k <- function(n) {
  return(floor(0.05*n))
}
=======
>>>>>>> a81bf9372f00c83572e6603694ed01578d028440:src/simulation.R
