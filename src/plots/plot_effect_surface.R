suppressMessages(library("magrittr"))
suppressMessages(library("assertthat"))
source("src/simulation/dgp.R")
source("src/predictor/treatment_effect_predictor.R")
source("src/predictor/conditional_mean_predictor.R")


LOWER_UPPER <- list(
  "complex"=c(0, 1),
  "boundary"=c(-3, 3),
  "simple"=c(-3, 3),
  "unbalanced"=c(-3, 3)
)


plot_surface <- function(
  method,
  version,
  treatment=TRUE,
  n=1000,
  d=2,
  p=1,
  interaction=FALSE,
  k=5,
  n_trees=500,
  n_threads=1,
  seed=0,
  noise_sd=1,
  n_ticks=50,
  theta_phi=c(300, 20),
  save=FALSE,
  file_name="bld/figures",
  load=TRUE
) {
  #' @description Plot predicted treatment effect of specified estimator.
  #' 
  #' @param method: Method used for the estimation of the treatment effects.
  #' Currently supported methods {'grf', 'knn', 'trf', 'const'}.
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
  #' @param n_ticks: Positive integer denoting the number of ticks used when
  #' constructing the mesh grid. More ticks result in a finer plot.
  #' @param theta_phi: 2d vector of positive integer, representing the degrees
  #' (horizontal, vertical) when visualizing the 3d effect.
  #' @param save: Logical, if TRUE the plot is saved.
  #' @param file_name: Character vector used for saving the plot if save is TRUE
  #' @param load: Logical, if TRUE we search for a plot which was
  #' created before in the directory specified by file_name and display this,
  #' otherwise a new plot is created.
  assertthat::assert_that(method %in% c(
    'const', 'rf', 'knn', 'trf', 'grf', 'ols', 'llf'
    ))
  assertthat::assert_that(
    version %in% c('simple', 'unbalanced', 'boundary', 'complex')
    )
  assertthat::assert_that(n %>% is.count)
  assertthat::assert_that(d %>% is.count)
  assertthat::assert_that(seed %>% is.number)
  assertthat::assert_that(noise_sd %>% is.scalar)
  assertthat::assert_that(n_ticks %>% is.count)
  assertthat::assert_that(length(theta_phi) == 2)
  assertthat::assert_that(save %>% is.logical)
  assertthat::assert_that(load %>% is.logical)
  assertthat::assert_that(file_name %>% is.null | file_name %>% is.string)
  
  if (version == "boundary" & treatment) {
    treatment <- FALSE
  }
  
  file_path <- create_file_path(
    file_name, method, version, n, d, noise_sd, p, k
    )
  
  # if the image has been created before, load, else create
  if (load & file.exists(file_path) & method %in% c("trf", "grf")) {
      figure <- png::readPNG(file_path)
      grid::grid.raster(figure)
  } else {
  
    plot_list <- plot_predicted_effect(
      method=method,
      version=version,
      treatment=treatment,
      n=n,
      d=d,
      p=p,
      interaction=interaction,
      k=k,
      n_trees=n_trees,
      n_threads=n_threads,
      seed=seed,
      noise_sd=noise_sd,
      n_ticks=n_ticks,
      theta_phi=theta_phi
      )
    
    # actual plotting
    if (save) {
      png(file_path, width=800, height=400)
      par(mfrow=c(1, 2))
      plot_list[["plot_call"]]()
      plot_true_effect(version, treatment, n_ticks)
      # mtext(plot_list[["plot_title"]], outer=TRUE, cex=1.5)
      dev.off()
    } else {
      par(mfrow=c(1, 2))
      plot_list[["plot_call"]]()
      plot_true_effect(version, treatment, n_ticks)
      # mtext(plot_list[["plot_title"]], outer=TRUE, cex=1.5, side=3, line=-5)
    }
    par(mfrow=c(1, 1))
  }
}


plot_predicted_effect <- function(
  method,
  version,
  treatment=TRUE,
  n=1000,
  d=2,
  p=1,
  interaction=FALSE,
  k=5,
  n_trees=500,
  n_threads=1,
  seed=0,
  noise_sd=1,
  n_ticks=50,
  theta_phi=c(300, 20)
  ) {
  #'
  data <- dgp(version, n, d, seed, noise_sd)
  
  if (treatment) {
    predictor_func <- treatment_effect_predictor
  } else {
    predictor_func <- conditional_mean_predictor
  }
  
  predictor <- predictor_func(
    method, data, p=p, interaction=interaction, k=k, n_trees=n_trees, n_threads=n_threads
    )
  
  lower = LOWER_UPPER[[version]][1]
  upper = LOWER_UPPER[[version]][2]
  
  # create mesh-grid and transform to 2d array
  x <- seq(lower, upper, length.out=n_ticks)
  mesh <- pracma::meshgrid(x)
  X <- mesh[["X"]]
  Y <- mesh[["Y"]]
  xy <- meshgrid_to_2d(X, Y)
  
  # extend 2d array to account for higher dimensionality
  nn <- n_ticks ** 2
  if (version == "complex") {
    sampler <- runif
  } else {
    sampler <- rnorm
  }
  extension <- matrix(sampler((d-2) * nn), nrow=nn, ncol=d-2)
  Xnew <- cbind(xy, extension)
  
  # predict effects and transform to 2d
  if (treatment) {
    effects <- predict_treatment_effect(
      method, predictor, X=Xnew, W=NULL, Y=NULL
    )
    zlab="(pred.) treatment effect"
  } else {
    effects <- predict_conditional_mean(
      method, predictor, X=Xnew, Y=NULL
    )
    zlab="(pred.) conditional mean"
  }
  effects2d <- pracma::Reshape(effects, n_ticks, n_ticks)
  
  # since const. is constant, function persp3D sets wrong limits
  if (method == "const") {
    zlim <- c(0, 4)
  } else {
    zlim <- range(effects2d, finite=TRUE)
  }
  # actual plot
  plot_call <- function() {
    GA::persp3D(
      x, x, effects2d, theta=theta_phi[1], phi=theta_phi[2],
      col.palette=GA::bl2gr.colors, expand=0.75, zlim=zlim,
      xlab="x1", ylab="x2", zlab=zlab
    )
  }
  plot_title <- create_plot_title(
    predictor, method, version, n, d, noise_sd, p, k
  )
  
  out <- list("plot_call"=plot_call, "plot_title"=plot_title)
  return(out)
}


plot_true_effect <- function(version, treatment=TRUE, n_ticks=50) {
  options <- switch (version,
    "complex" = list(treatment_effect_complex, c(0, 4)),
    "boundary" = list(main_effect_boundary, c(-4, 8)),
    "simple" = list(treatment_effect_simple, c(0, 4)),
    "unbalanced" = list(treatment_effect_unbalanced, c(-2, 10))
  )
  
  func = options[[1]]
  zlim = options[[2]]
  lower = LOWER_UPPER[[version]][1]
  upper = LOWER_UPPER[[version]][2]
  
  if (treatment) {
    zlab <- "(true) treatment effect"
  } else {
    zlab <- "(true) conditional mean"
  }
  
  x <- seq(lower, upper, length.out=n_ticks)
  y <- x
  z <- outer(x, y, function(x, y) func(cbind(x, y)))
  GA::persp3D(
    x, y, z, theta=300, phi=20, col.palette=GA::bl2gr.colors, expand=0.75,
    zlim=zlim, xlab="x1", ylab="x2", zlab=zlab
    )
}


meshgrid_to_2d <- function(X, Y) {
  n <- dim(X)[1]
  dim(X) <- c(n, n, 1)
  dim(Y) <- c(n, n, 1)
  XY3d <- abind::abind(X, Y, along=3)
  XY2d <- pracma::Reshape(XY3d, n**2, 2)[, c(2, 1)]
  return(XY2d)
}


create_file_path <- function(
  file_name, method, version, n, d, noise_sd, p, k
  ) {
  # construct image identifier
  noise_sd <- gsub("\\.", "-", noise_sd)
  if (method == "ols") {
    identifier <- paste0(
      paste(method, version, n, d, noise_sd, p, sep="_"), ".png"
      )
  } else if (method == "knn") {
    identifier <- paste0(
      paste(method, version, n, d, noise_sd, k, sep="_"), ".png"
      )
  } else {
    identifier <- paste0(
      paste(method, version, n, d, noise_sd, sep="_"), ".png"
      )
  }
  file_path <- ifelse(
    is.null(file_name),
    identifier, paste0(file_name, "/", identifier)
    )
  
  return(file_path)
}


create_plot_title <- function(
  predictor, method, version, n, d, noise_sd, p, k
) {
  plot_title <- paste0(
      "method: ", method, " version: ", version, " n: ", n, " d: ", d,
      " noise: ", noise_sd
  )
  if (method == "knn") {
    k <- predictor[["k"]]
    plot_tile <- paste0(plot_title, " k: ", k)
  } else if (method == "ols") {
    p <- predictor[["p"]]
    plot_title <- paste0(plot_title, " p: ", p)
  }
  
  return(plot_title)
}
