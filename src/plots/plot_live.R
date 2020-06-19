# Functions to plot the treatment effects functions used in the dgp's defined
# in the script ``src/dgp.R``.

suppressMessages(library("ggplot2"))
suppressMessages(library("plotly"))
suppressMessages(library("tibble"))
suppressMessages(library("pracma"))
suppressMessages(library("abind"))
source("src/simulation/dgp.R")

plot_live <- function(version) {
  #' Return plot of treatment or main effect function for various processes.
  #'
  #' @param version: String determining which dgp is used. Implemented dgp's
  #' are {'complex_treatment', 'boundary', 'unbalanced'}.
  #' 
  #' @return The constructed plot.
  #' 
  #' @examples
  #' fig <- plot_dgp("complex_treatment")
  #' fig
  
  fig <- switch (version,
    "complex" = plot_treatment_complex(),
    "unbalanced" = plot_treatment_unbalanced(),
<<<<<<< HEAD:src/plots/plot_live.R
    "boundary" = plot_main_boundary(),
=======
    "boundary" = plot_treatment_boundary(),
>>>>>>> a81bf9372f00c83572e6603694ed01578d028440:src/plot_treatment.R
    "simple" = plot_treatment_simple(),
    "simple_main" = plot_main_simple(),
    "unbalanced_main" = plot_main_unbalanced(),
    stop(sprintf("%s is not in set of implemented dgps: {'complex_treatment',
                  'boundary', 'unbalanced', 'simple'}", version))
  )
  return(fig)
}


#-----------------------------Specific processes-------------------------------


plot_treatment_simple <- function() {
  #' Plot treatment effect for simple setting with two dimensional features.
  n_points <- 100
  mesh <- custom_meshgrid(lower=0, upper=1, n_points=n_points)
  
  treatment_effect <- treatment_effect_simple(mesh$XY)
  treatment_mesh <- matrix(treatment_effect, ncol=n_points)
  
  axx <- list(nticks=10, range=c(0, 1), title="X1")
  axy <- list(nticks=10, range=c(0, 1), title="X2")
  axz <- list(nticks=10, range=c(0, 4), title="treatment effect")
  aspectratio <- list(x=1, y=1, z=0.75)
  scene <- list(xaxis=axx, yaxis=axy, zaxis=axz, aspectratio=aspectratio)
  
  fig <- plotly::plot_ly(
    type="surface",
    x=~mesh$X, y=~mesh$Y, z=~treatment_mesh,
    showscale=FALSE
    )
  fig <- fig %>% plotly::layout(scene=scene)
  
  return(fig)
}


plot_main_simple <- function() {
  #' Plot treatment effect for simple setting with two dimensional features.
  n_points <- 100
  mesh <- custom_meshgrid(lower=0, upper=1, n_points=n_points)
  
  treatment_effect <- main_effect_simple(mesh$XY)
  treatment_mesh <- matrix(treatment_effect, ncol=n_points)
  
  axx <- list(nticks=10, range=c(0, 1), title="X1")
  axy <- list(nticks=10, range=c(0, 1), title="X2")
  axz <- list(nticks=10, range=c(0, 4), title="main effect")
  aspectratio <- list(x=1, y=1, z=0.75)
  scene <- list(xaxis=axx, yaxis=axy, zaxis=axz, aspectratio=aspectratio)
  
  fig <- plotly::plot_ly(
    type="surface",
    x=~mesh$X, y=~mesh$Y, z=~treatment_mesh,
    showscale=FALSE
    )
  fig <- fig %>% plotly::layout(scene=scene)
  
  return(fig)
}


plot_treatment_complex <- function() {
  #' Plot treatment effect for complex setting with two dimensional features.
  n_points <- 100
  mesh <- custom_meshgrid(lower=0, upper=1, n_points=n_points)
  
  treatment_effect <- treatment_effect_complex(mesh$XY)
  treatment_mesh <- matrix(treatment_effect, ncol=n_points)
  
  axx <- list(nticks=10, range=c(0, 1), title="X1")
  axy <- list(nticks=10, range=c(0, 1), title="X2")
  axz <- list(nticks=10, range=c(0, 4), title="treatment effect")
  aspectratio <- list(x=1, y=1, z=0.75)
  scene <- list(xaxis=axx, yaxis=axy, zaxis=axz, aspectratio=aspectratio)
  
  fig <- plotly::plot_ly(
    type="surface",
    x=~mesh$X, y=~mesh$Y, z=~treatment_mesh,
    showscale=FALSE
    )
  fig <- fig %>% plotly::layout(scene=scene)
  
  return(fig)
}


plot_main_unbalanced <- function() {
  #' Plot main effect for unbalanced setting with two dimensional features.
  n_points <- 200
  mesh <- custom_meshgrid(lower=-5, upper=5, n_points=n_points)
  
  treatment_effect <- main_effect_unbalanced(mesh$XY, 2)
  treatment_mesh <- matrix(treatment_effect, ncol=n_points)
  
  axx <- list(nticks=10, range=c(-2, 2), title="X1")
  axy <- list(nticks=10, range=c(-2, 2), title="X2")
  axz <- list(nticks=10, range=c(0, 8), title="treatment effect")
  aspectratio <- list(x=1, y=1, z=0.75)
  scene <- list(xaxis=axx, yaxis=axy, zaxis=axz, aspectratio=aspectratio)
  
  fig <- plotly::plot_ly(
    type="surface",
    x=~mesh$X, y=~mesh$Y, z=~treatment_mesh,
    showscale=FALSE
    )
  fig <- fig %>% plotly::layout(scene=scene)
  
  return(fig)
}

plot_treatment_unbalanced <- function() {
  #' Plot treatment effect for unbalanced setting with two dimensional features.
  n_points <- 200
  mesh <- custom_meshgrid(lower=-5, upper=5, n_points=n_points)
  
  treatment_effect <- treatment_effect_unbalanced(mesh$XY)
  treatment_mesh <- matrix(treatment_effect, ncol=n_points)
  
  axx <- list(nticks=10, range=c(-5, 5), title="X1")
  axy <- list(nticks=10, range=c(-5, 5), title="X2")
  axz <- list(nticks=10, range=c(0, 8), title="treatment effect")
  aspectratio <- list(x=1, y=1, z=0.75)
  scene <- list(xaxis=axx, yaxis=axy, zaxis=axz, aspectratio=aspectratio)
  
  fig <- plotly::plot_ly(
    type="surface",
    x=~mesh$X, y=~mesh$Y, z=~treatment_mesh,
    showscale=FALSE
    )
  fig <- fig %>% plotly::layout(scene=scene)
  
  return(fig)
}


plot_main_boundary <- function() {
  #' Plot main effect for boundary setting with two dimensional features.
  n_points <- 200
  mesh <- custom_meshgrid(lower=-5, upper=5, n_points=n_points)
  
  outcomes <- main_effect_boundary(mesh$XY)
  treatment_mesh <- matrix(outcomes, ncol=n_points)
  
  axx <- list(nticks=10, range=c(-5, 5), title="X1")
  axy <- list(nticks=10, range=c(-5, 5), title="X2")
  axz <- list(nticks=10, range=c(-5, 10), title="treatment effect")
  aspectratio <- list(x=1, y=1, z=0.75)
  scene <- list(xaxis=axx, yaxis=axy, zaxis=axz, aspectratio=aspectratio)
  
  fig <- plotly::plot_ly(
    type="surface",
    x=~mesh$X, y=~mesh$Y, z=~treatment_mesh,
    showscale=FALSE
    )
  fig <- fig %>% plotly::layout(scene=scene)
  
  return(fig)
}



#---------------------------Helper functions------------------------------------

custom_meshgrid <- function(lower, upper, n_points) {
  # construct meshgrid
  linegrid <- seq(lower, upper, length.out=n_points)
  mesh <- pracma::meshgrid(linegrid, linegrid)
  X <- mesh$X
  Y <- mesh$Y
  
  # meshgrid to 2d array
  XY <- abind::abind(X, Y, along=3)
  XY <- matrix(XY, ncol=2)
  
  out <- list(XY=XY, X=X, Y=Y)
  return(out)
}
