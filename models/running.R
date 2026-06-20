library(Maeswrap)
library(tidyverse)
library(ggplot2)
library(purrr)

source("Function_M.R")

options(dplyr.summarise.inform = FALSE)

# The DEEP CROWN setup adds rnorm() jitter to crown heights (Function_M.R), so
# the approximations are stochastic. Seed once here for reproducible figures.
set.seed(20240620)

# Build the final results table for one experiment: load the MAESPA ground-truth
# csv, run the canopy approximations over the matching simulation folders, average
# the replicates, and stack the two together (arranged by stand name + model).
#
#   csv              - MAESPA output csv for this experiment
#   folder           - simulation folder to read the stands from
#   grid             - named list of the parameter grid (H, V, L, F, fla, S)
#   per_leaf_area    - express absPAR per unit leaf area (TRUE) or per stand (FALSE)
#   stand            - whole-stand experiment rather than single focal tree
#   max_n            - cap on the number of stands processed
#   maespa_transform - optional tweak applied to the MAESPA data before binding
build_experiment <- function(csv, folder, grid,
                             per_leaf_area = TRUE, stand = FALSE, max_n = 1e4,
                             maespa_transform = identity) {
  maespa <- load_maespa_results(csv, per_leaf_area = per_leaf_area) %>%
    maespa_transform()

  results <- do.call(
    process_experiment,
    c(list(path = folder, stand = stand, max_n = max_n), grid)
  )

  averaged <- calculate_averages(results, per_leaf_area = per_leaf_area)

  bind_rows(averaged, maespa) %>%
    arrange(name, model)
}

# focal-tree height : stand ratios, shared by the competition and self-shading grids
F_ratios <- c(1.99, 1.85, 1.75, 1.60, 1.50, 1.35, 1.25, 1.15, 1.05, 1.00,
              0.95, 0.85, 0.75, 0.65, 0.50, 0.40, 0.25, 0.15, 0.01)

# --- Competition: focal LA 0.1 ------------------------------------------------
final_results_fla_0.1 <- build_experiment(
  csv    = "Different maespa csv/maespa_fla_0.1.csv",
  folder = "simulations_fla_0.1",
  grid   = list(H = 15, V = c(0, 0.1, 0.25, 0.5),
                L = c(0.44, 1.521, 2.916, 4.556, 5.402),
                F = F_ratios, fla = 0.1, S = c(1, 2, 3)),
  max_n  = 1140
)

# --- Competition: focal LA 27 -------------------------------------------------
final_results_fla_27 <- build_experiment(
  csv    = "Different maespa csv/maespa_fla_27.csv",
  folder = "simulations_fla_27",
  grid   = list(H = 15, V = c(0, 0.1, 0.25, 0.5),
                L = c(0.44, 1.521, 2.916, 4.556, 5.402),
                F = F_ratios, fla = 27, S = c(1, 2, 3))
)

# --- Stand interception: focal LA 0.1 -----------------------------------------
final_results_stand_fla_0.1 <- build_experiment(
  csv           = "Different maespa csv/maespa_stand_fla_0.1.csv",
  folder        = "simulations_stand_fla_0.1",
  grid          = list(H = 15, V = c(0, 0.1, 0.25, 0.5),
                       L = c(0.44, 1.521, 2.916, 4.556, 5.402),
                       F = 1.0, fla = 0.1, S = c(1, 2, 3)),
  per_leaf_area = FALSE,
  stand         = TRUE,
  max_n         = 60
)

# --- Stand interception: focal LA 27 ------------------------------------------
# The MAESPA stand-27 absPAR is scaled by 1.35 to match the approximations' basis.
final_results_stand_fla_27 <- build_experiment(
  csv              = "Different maespa csv/maespa_stand_fla_27.csv",
  folder           = "simulations_stand_fla_27",
  grid             = list(H = 15, V = c(0, 0.1, 0.25, 0.5),
                          L = c(0.44, 1.521, 2.916, 4.556, 5.402),
                          F = 1.0, fla = 27, S = c(1, 2, 3)),
  per_leaf_area    = FALSE,
  stand            = TRUE,
  max_n            = 60,
  maespa_transform = function(d) mutate(d, absPAR_two_s = absPAR_two_s * 1.35)
)

# --- Self-shading: focal LA varied --------------------------------------------
final_results_c_fla <- build_experiment(
  csv    = "Different maespa csv/maespa_c_fla.csv",
  folder = "simulations_c_fla",
  grid   = list(H = 15, V = 0.1, L = 2.916,
                F = F_ratios, fla = c(0.1, 0.5, 1, 10, 20, 40), S = c(1, 2, 3))
)
