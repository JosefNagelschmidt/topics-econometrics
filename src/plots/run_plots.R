#' Script to create multiple plots and write them to disc, using methods in
#' ``src/plots/plot_effect_surface.R``.

suppressMessages(library("rjson"))
suppressMessages(library("magrittr"))
suppressMessages(library("stringr"))
suppressMessages(library("readr"))
suppressMessages(library("dplyr"))
suppressMessages(library("future"))
suppressMessages(library("rlist"))
source("src/plots/plot_effect_surface.R")


# load options
options <- rjson::fromJSON(
  file="src/plots/plot_config.json"
)

# set options for parallel processing
if (options[["parallel"]]) {
  plan(tweak(multiprocess, workers=options[["n_workers"]]))
}

# meta options
dir <- options[["dir"]]

# inputs
method_list <- options[["method_list"]]
version_list <- options[["version_list"]]
n_list <- options[["n_list"]]
d_list <- options[["d_list"]]
noise_sd_list <- options[["noise_sd_list"]]


construct_grid <- function(
  method_list, version_list, n_list, d_list, noise_sd_list, options
) {
  #' Construct parameter grid as list for plotting purposes.
  columns <- c("version", "n", "d", "noise_sd")
  
  params <- list()
  for (method in method_list) {
    grid <- tidyr::expand_grid(
      version_list, n_list, d_list, noise_sd_list
    )
    colnames(grid) <- columns
    
    grid <- grid %>% tibble::add_column("method"=method)
    
    params[[method]] <- grid %>% split(seq(nrow(grid)))
  }
  
  params_grid <- unlist(params, recursive=FALSE)
  return(params_grid)
}


params_grid <- construct_grid(
  method_list, version_list, n_list, d_list, noise_sd_list, options
  )

# save=TRUE
func <- function(...) {
  plot_surface(
    save=TRUE, load=FALSE, file_name=dir, n_threads=2, ...
    )
}

# plot in parallel
# for (kwargs in params_grid) {
#   do.call(func, kwargs)
# }
furrr::future_map(
  params_grid,
  function(kwargs) do.call(func, kwargs),
  .progress=TRUE
  )

