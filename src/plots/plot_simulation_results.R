#' Run a Monte Carlo simulation for specified parameters using function
#' ``monte_carlo_simulation`` from ``src/simulation/simulation.R``. Parameters
#' are specified in the json file ``src/simulation/simulation_config.json``.

suppressMessages(library("magrittr"))
suppressMessages(library("readr"))
suppressMessages(library("ggplot2"))
suppressMessages(library("dplyr"))
suppressMessages(library("extrafont"))

produce_plots <- function(treatment, noise) {
  csv_file_name <- ifelse(
    treatment,
    "bld/data/simulation_treatment.csv",
    "bld/data/simulation_conditional_mean.csv"
    )
  
  # load csv
  df <- readr::read_csv(csv_file_name)
  version_list <- unique(df[["version"]])
  
  if (treatment) {
    df[["method"]] <- factor(df[["method"]], levels=c("const", "ols", "knn", "grf", "trf"))
  } else {
    df[["method"]] <- factor(df[["method"]], levels=c("ols", "knn", "rf", "grf", "llf"))
    df <- df %>% filter(!(method=="ols"))
  }
  
  # construct plot
  
  for (v in version_list) {
    if (!(v %in% df[["version"]])) next
    if (noise) {
      df_tmp <- df %>% filter(version==v) #, noise_sd==1)
    } else {
      df_tmp <- df %>% filter(version==v, noise_sd==1)
    }
    p <- ggplot2::ggplot(df_tmp, aes(y=mae, x=method, fill=method))
    if (noise) {
      p <- p + geom_point(aes(alpha=factor(noise_sd)), size=5, pch=21) + 
      scale_alpha_manual(values=c(0.2, 0.85), name="noise")
    } else {
      p <- p + geom_point(size=5, pch=21)
    }
    p <- p + facet_grid(rows=vars(n), cols=vars(d)) + 
      ylim(0, 2) + 
      theme_minimal() + 
      theme(
        text=element_text(family="Palatino", face="italic"),
        plot.title = element_text(vjust=-2.5),
        panel.spacing = unit(0.8, "lines"),
        panel.grid.minor.y = element_line(colour = "black", size=0.1),
        panel.grid.major.y = element_line(colour = "black", size=0.2),
        panel.grid.minor.x = element_line(colour = "black", size=0.05),
        panel.grid.major.x = element_line(colour = "black", size=0.05)
        ) + 
      xlab("") + 
      ylab("(integrated) mean absolute error") +
      ggtitle(paste0("dgp: ", v))
    
    if (noise) {
      file <- paste0("bld/simulation_plots/", v, "-noise.png")
    } else{
      file <- paste0("bld/simulation_plots/", v, ".png")
    }
    ggsave(file, p, device="png")
  }
}


for (treatment in c(TRUE, FALSE)) {
  for (noise in c(TRUE, FALSE)) {
    produce_plots(treatment, noise)
  }
}