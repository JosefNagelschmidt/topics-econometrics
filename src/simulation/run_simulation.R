#' Run a Monte Carlo simulation for specified parameters using function
#' ``monte_carlo_simulation`` from ``src/simulation/simulation.R``. Parameters
#' are specified in the json file ``src/simulation/simulation_config.json``.
#' 
#' When executing this file two simulations are run
#' 
#' 1. Treatment effect estimation simulation
#' 2. Classical regression estimation
#' 

suppressMessages(library("rjson"))
suppressMessages(library("magrittr"))
suppressMessages(library("stringr"))
suppressMessages(library("readr"))
suppressMessages(library("future"))
source("src/simulation/simulation.R")


run_simulation <- function(treatment, simulation_options, options) {
  # set options for parallel processing
  if (options[["parallel"]]) {
    plan(tweak(multiprocess, workers=options[["n_workers"]]))
  }
  
  # inputs
  n_sim <- options[["n_sim"]]
  method_list <- simulation_options[["method_list"]]
  version_list <- simulation_options[["version_list"]]
  n_list <- simulation_options[["n_list"]]
  d_list <- simulation_options[["d_list"]]
  noise_sd_list <- simulation_options[["noise_sd_list"]]
  
  # run simulation
  result <- monte_carlo_simulation(
    n_sim=n_sim,
    method_list=method_list,
    version_list=version_list,
    n_list=n_list,
    d_list=d_list,
    noise_sd_list=noise_sd_list,
    treatment=treatment,
    verbose=FALSE,
    progress_bar=TRUE
  )
  
  # store results if wanted
  file_name <- ifelse(treatment,
                      "bld/data/simulation_treatment.csv",
                      "bld/data/simulation_conditional_mean.csv"
  )
  if (options[["save_simulation"]]) {
    readr::write_csv(result, file_name)
  }
    
}


for (treatment in c(TRUE, FALSE)) {
  # load options
  file_name <- ifelse(treatment,
                 "src/simulation/simulation_treatment_config.json",
                 "src/simulation/simulation_conditional_mean_config.json"
                 )
  simulation_options <- rjson::fromJSON(
    file=file_name
    )
  options <- simulation_options[["options"]]
  if (options[["run"]]) {
    run_simulation(treatment, simulation_options, options)
  }
}
