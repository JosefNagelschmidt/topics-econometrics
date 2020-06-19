#' Run a Monte Carlo simulation for specified parameters using function
#' ``monte_carlo_simulation`` from ``src/simulation/simulation.R``. Parameters
#' are specified in the json file ``src/simulation/simulation_config.json``.

suppressMessages(library("magrittr"))
suppressMessages(library("readr"))
suppressMessages(library("ggplot2"))

<<<<<<< HEAD
produce_plots <- function(treatment, noise) {
  csv_file_name <- ifelse(
    treatment,
    "bld/data/simulation_treatment.csv",
    "bld/data/simulation_conditional_mean.csv"
    )
  
  # load csv
  df <- readr::read_csv(csv_file_name)
  version_list <- unique(df[["version"]])
  
  # construct plot
  
  for (v in version_list) {
    if (!(v %in% df[["version"]])) next
    df_tmp <- df %>% filter(version==v) #, noise_sd==1)
    p <- ggplot2::ggplot(df_tmp, aes(y=mae, x=method, fill=method)) + 
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
        text=element_text(family="palatino", face="italic"),
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
=======
# load options
simulation_options <- rjson::fromJSON(
  file="src/simulation/simulation_config.json"
  )
version_list <- simulation_options[["version_list"]]

# load csv
df <- readr::read_csv("bld/data/simulation_results_const_ols_knn_trf_grf.csv")

df <- df %>% filter(mae <= 5)
# construct plot

for (v in version_list) {
  if (!(v %in% df[["version"]])) next
  df_tmp <- df %>% filter(version==v)
  p <- ggplot2::ggplot(df_tmp, aes(y=mae, x=method, fill=method)) + 
    geom_point(size=5, pch=21) + 
    facet_grid(rows=vars(n), cols=vars(d)) + 
    ylim(0, 2) + 
    theme_minimal() + 
    theme(
      panel.spacing=unit(.2, "lines"),
      panel.border = element_rect(color = "black", fill = NA, size = 0.7),
      strip.background = element_blank()
      ) + 
  
    xlab("") + 
    ggtitle(paste0("dgp: ", v))
  p
  
  file <- paste0("bld/simulation_plots/", v, ".png")
  ggsave(file, p, device="png")
>>>>>>> a81bf9372f00c83572e6603694ed01578d028440
}
