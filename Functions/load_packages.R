required_packages <- c(
  "tidyverse",
  "lme4",
  "car",
  "nlme",
  "emmeans",
  "gridExtra",
  "DHARMa",
  "glmmTMB",
  "ggpubr",
  "patchwork"
)


install_if_missing <- function(pkgs) {
  
  missing <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
  
  if (length(missing) > 0) {
    install.packages(missing)
  }
}


load_packages <- function(pkgs) {
  
  invisible(
    lapply(pkgs, library,
           character.only = TRUE)
  )
}


install_if_missing(required_packages)
load_packages(required_packages)

