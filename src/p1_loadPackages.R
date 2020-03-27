### Function to install new packages
ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE)
  }
  sapply(pkg, require, character.only = TRUE)
}

# Required packages
packages <- c(
  "ggplot2", "lubridate", "maptools", "circular", "lattice", "fossil",
  "RColorBrewer", "rgdal", "sp", "raster", "here", "readr", "spatialEco", "zoo",
  "RNCEP", "reshape2", "gridExtra", "lme4", "MASS", "car", "emmeans"
)

# Load/install packages
ipak(packages)
