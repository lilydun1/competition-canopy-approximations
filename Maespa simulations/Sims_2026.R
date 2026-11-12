## Sims_2026.R
## ---------------------------------------------------------------------------
## Rerun of the MAESPA stands used in the paper (individual competition with a
## 0.1 m² focal tree; self-shading with focal leaf area varied), correcting two
## problems with the first round (Sims.R):
##   1. plot area: the number of trees was set from a 12,000 m² plot but trees
##      were placed over 196 x 196 m, so realised LAI was ~0.3x the intended LAI;
##   2. MAESPA's MAXT = 1500 tree limit truncated and scrambled the two densest
##      stands (see src/README.md).
## Stands are now complete n x n grids on the 196 m plot, with n chosen so the
## realised LAI (27 m² / spacing²) is close to the intended values. Everything
## else (trees, edge wall, height CVs, seeds, focal-height ratios) is as before.
##
## Run from this folder:  Rscript Sims_2026.R   (uses parallel::mclapply)
## ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(Maeswrap)
  library(tidyverse)
})
source("R/Function.R")

Sys.setenv(MAESPA_BIN = normalizePath("src/maespa-maxt10000"))
n_cores <- max(1, parallel::detectCores() - 2)

## trees per row -> realised LAI = 27 / (196 / (n - 1))^2
stand_grid <- tibble(
  n_side = c(26, 46, 66, 79, 89),
  L = round(27 / (196 / (n_side - 1))^2, 2)       # 0.44 1.42 2.97 4.28 5.44
)

F_ratios <- c(1.99, 1.85, 1.75, 1.60, 1.50, 1.35, 1.25, 1.15, 1.05, 1.00,
              0.95, 0.85, 0.75, 0.65, 0.50, 0.40, 0.25, 0.15, 0.01)

build_and_run <- function(combos, folder) {
  combos <- combos %>%
    mutate(path = sprintf("%s/H%s_V%s_L%s_F%s_fla%s_S%s", folder, H, V, L, F, fla, S))
  message(folder, ": building ", nrow(combos), " stands")
  invisible(parallel::mclapply(seq_len(nrow(combos)), function(i) {
    with(combos[i, ], create_simulation(path = path, template = "template_A",
         h_mn = H, h_cv = V, ft_h = F, fla = fla, seed = S, n_side = n_side))
  }, mc.cores = n_cores))
  message(folder, ": running MAESPA")
  invisible(parallel::mclapply(combos$path, run_simulation, mc.cores = n_cores))

  ## guard against the MAXT problem and failed runs
  exceeded <- combos$path[map_lgl(combos$path, ~ any(grepl("EXCEEDED MAXIMUM",
                 readLines(file.path(.x, "Maeserr.dat"), warn = FALSE))))]
  missing <- combos$path[!file.exists(file.path(combos$path, "Dayflx.dat"))]
  if (length(exceeded) || length(missing))
    stop("MAESPA problems: ", length(exceeded), " exceeded MAXT, ",
         length(missing), " without output")

  combos %>%
    left_join(map_df(combos$path, load_output), by = "path") %>%
    group_by(H, V, L, n_side, F, fla, Tree, name) %>%
    summarise(across(c(absPAR, absNIR, absTherm, totPs, netPs, totRf, totLE1, totLE2, totH), mean),
              .groups = "drop")
}

## Individual competition: focal leaf area 0.1 m², 5 densities x 4 CVs x 19 ratios x 3 seeds
comp <- expand_grid(H = 15, V = c(0, 0.1, 0.25, 0.5), stand_grid,
                    F = F_ratios, fla = 0.1, S = 1:3)
out_comp <- build_and_run(comp, "simulations2026_fla_0.1")
write_csv(out_comp, "maespa2026_fla_0.1.csv")

## Self-shading: LAI ~2.98, CV 0.1, focal leaf area varied
ss <- expand_grid(H = 15, V = 0.1, filter(stand_grid, n_side == 66),
                  F = F_ratios, fla = c(0.1, 0.5, 1, 10, 20, 40), S = 1:3)
out_ss <- build_and_run(ss, "simulations2026_c_fla")
write_csv(out_ss, "maespa2026_c_fla.csv")

file.copy(c("maespa2026_fla_0.1.csv", "maespa2026_c_fla.csv"),
          "../models/Different maespa csv", overwrite = TRUE)
message("done")
