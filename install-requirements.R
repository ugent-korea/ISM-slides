# Create writable directory for R packages
pkgs_dir <- Sys.getenv("R_LIBS_USER")
dir.create(pkgs_dir, showWarnings = FALSE)

# Install R packages
packages <- c(
    "GGally",
    "MASS",
    "car",
    "caret",
    "faraway",
    "ggplot2",
    "glmnet",
    "gridExtra",
    "latex2exp",
    "mvtnorm",
    "pROC",
    "plot3D",
    "pls",
    "scales",
    "tidyverse"
)
install.packages(packages, dependencies = TRUE, lib = pkgs_dir)