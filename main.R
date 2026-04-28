# =============================================================================
# CUNEF University – Research Assistant Task (EFF 2017)
# main.R – Master script: sources all analysis scripts in order
# =============================================================================

# Requires RStudio to resolve the project root automatically via .Rproj.
# Alternatively, set the working directory manually:
# setwd("/Users/stefan/VSCode/CUNEF-Coding")

if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

source("R/00_setup.R")               # packages, paths, data, helpers
source("R/01_implicates.R")          # Q1: implicates & Rubin's rules
source("R/02_descriptives.R")        # Q2: descriptive statistics
source("R/03_homeownership_models.R") # Q3: homeownership models
source("R/04_merge_proxy.R")         # Q4: merge + proxy variable
source("R/05_constructed_vars.R")    # Q5: constructed variables & comparisons
source("R/06_mortgage_regressions.R") # Q6: mortgage regressions + LaTeX table

message("\n===== All scripts completed successfully =====\n")
