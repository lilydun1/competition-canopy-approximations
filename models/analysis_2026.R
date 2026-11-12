## analysis_2026.R
## ---------------------------------------------------------------------------
## Stage-1 analysis for the paper, from the 2026 MAESPA rerun
## (../Maespa simulations/Sims_2026.R). Calibrates the light coefficients,
## computes the three canopy approximations, the fit statistics, and the
## figures used in the manuscript and SI.
##
##   Rscript analysis_2026.R          (from models/)
## Optional: MS_FIG_DIR=<manuscript figures folder> also writes the figures there.
## ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(Maeswrap)
  library(tidyverse)
})
source("Function_M.R")
options(dplyr.summarise.inform = FALSE)
set.seed(20261113)

sims <- "../Maespa simulations"
fig_dir <- "figures"; dir.create(fig_dir, showWarnings = FALSE)
ms_dir <- Sys.getenv("MS_FIG_DIR")

F_ratios <- c(1.99, 1.85, 1.75, 1.60, 1.50, 1.35, 1.25, 1.15, 1.05, 1.00,
              0.95, 0.85, 0.75, 0.65, 0.50, 0.40, 0.25, 0.15, 0.01)
L_vals <- c(0.44, 1.42, 2.97, 4.28, 5.44)

## ---- MAESPA outputs (absorbed PAR per unit leaf area) -----------------------
read_maespa <- function(csv) read_csv(csv, show_col_types = FALSE) %>%
  transmute(H, V, L, F, fla, absPAR = absPAR / fla)
m_comp <- read_maespa(file.path(sims, "maespa2026_fla_0.1.csv"))
m_ss   <- read_maespa(file.path(sims, "maespa2026_c_fla.csv"))

## ---- approximations ----------------------------------------------------------
## LAI tables are slow to build (~10 min); cached in lai2026.rds (delete to rebuild)
if (file.exists("lai2026.rds")) {
  lai <- readRDS("lai2026.rds"); lai_comp <- lai$comp; lai_ss <- lai$ss
} else {
  message("computing LAI above focal trees ...")
  lai_comp <- approx_lai_2026(file.path(sims, "simulations2026_fla_0.1"),
    expand_grid(H = 15, V = c(0, 0.1, 0.25, 0.5), L = L_vals, F = F_ratios, fla = 0.1, S = 1:3))
  lai_ss <- approx_lai_2026(file.path(sims, "simulations2026_c_fla"),
    expand_grid(H = 15, V = 0.1, L = 2.97, F = F_ratios, fla = c(0.1, 0.5, 1, 10, 20, 40), S = 1:3))
  saveRDS(list(comp = lai_comp, ss = lai_ss), "lai2026.rds")
}

G <- daily_par("met.dat")
light  <- calibrate_light(lai_comp, m_comp, G, stream = 2)
light1 <- calibrate_light(lai_comp, m_comp, G, stream = 1)
message(sprintf("light model: a = %.3f, w = %.3f, ka = %.3f, kb = %.3f (single-component: k = %.3f)",
                light[["a"]], light[["w"]], light[["ka"]], light[["kb"]], light1[["k"]]))
write_csv(as_tibble(as.list(c(light, k_single = light1[["k"]]))), "light2026.csv")

combine <- function(lai_tab, maespa, stream = 2) {
  bind_rows(approx_par_2026(lai_tab, if (stream == 1) light1 else light, G, stream),
            maespa %>% mutate(model = "MAESPA")) %>%
    mutate(model = factor(model, levels = c("MAESPA", "PPA", "FLAT TOP", "DEEP CROWN")))
}
res_comp <- combine(lai_comp, m_comp)
res_ss   <- combine(lai_ss, m_ss)
write_csv(res_comp, "results2026_competition.csv")
write_csv(res_ss, "results2026_selfshading.csv")

## ---- fit statistics ----------------------------------------------------------
fit_stats <- function(res, label) {
  res %>% filter(model != "MAESPA") %>%
    inner_join(filter(res, model == "MAESPA") %>% select(-model),
               by = c("H", "V", "L", "F", "fla"), suffix = c("", ".maespa")) %>%
    group_by(model) %>%
    summarise(R2 = cor(absPAR, absPAR.maespa)^2,
              RMSE = sqrt(mean((absPAR - absPAR.maespa)^2)), n = n()) %>%
    mutate(experiment = label)
}
stats <- bind_rows(
  fit_stats(res_comp, "competition (all stands)"),
  fit_stats(filter(res_ss, fla == 0.1), "self-shading, focal LA 0.1"),
  fit_stats(filter(res_ss, fla == 40), "self-shading, focal LA 40")
)
stats1 <- fit_stats(combine(lai_comp, m_comp, stream = 1), "competition, single-component light model")
stats <- bind_rows(stats, stats1)
write_csv(stats, "stats2026.csv")
print(stats %>% mutate(across(c(R2, RMSE), ~ round(.x, 3))))

## ---- figures -----------------------------------------------------------------
cols <- c("grey45", "red", "#00BA38", "goldenrod2")
model_labels <- c("MAESPA", "PPA", "Flat Top", "Deep Crown")
theme_canopy <- theme(
  axis.ticks = element_line(colour = "grey"),
  axis.title = element_text(family = "Helvetica", colour = "black", size = 15),
  axis.text = element_text(family = "Helvetica", size = 11),
  legend.key = element_rect(fill = "white"),
  legend.title = element_text(family = "Helvetica", colour = "black", size = 15),
  legend.text = element_text(family = "Helvetica", colour = "black", size = 13),
  panel.border = element_rect(colour = "grey45", fill = NA),
  panel.grid.major = element_line(colour = "grey", linewidth = 0.1),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "white"),
  strip.background = element_rect(fill = "white"),
  strip.text = element_text(family = "Helvetica", colour = "black", size = 13),
  plot.tag = element_text(family = "Helvetica", size = 16))
x_ratio <- scale_x_continuous(breaks = c(0, 0.5, 1, 1.5, 2))
lab_par <- labs(x = "\nRatio focal tree height : stand",
                y = expression(atop("Absorbed PAR", (MJ~m^-2~d^-1))),
                colour = "Canopy Approximation")
scale_models <- scale_colour_manual(labels = model_labels, values = cols)

save_fig <- function(plot, name, width, height) {
  dirs <- c(fig_dir, if (nzchar(ms_dir)) ms_dir)
  for (d in dirs) for (ext in c("png", "pdf"))
    ggsave(file.path(d, paste0(name, ".", ext)), plot, width = width, height = height, dpi = 300)
}

lai_lab <- as_labeller(c(setNames(paste("LAI =", formatC(L_vals, format = "f", digits = 2)), L_vals),
                         `0` = "CV = 0", `0.1` = "CV = 0.1", `0.25` = "CV = 0.25", `0.5` = "CV = 0.5"))

## Fig. compositions: all stands
save_fig(
  ggplot(res_comp, aes(F, absPAR, colour = model)) + geom_line() +
    facet_grid(rows = vars(V), cols = vars(L), labeller = lai_lab) +
    lab_par + scale_models + x_ratio + theme_canopy,
  "competition_fla_0.1", 11, 8)

## Fig. individual: one stand (LAI 2.97, CV 0.25)
save_fig(
  ggplot(filter(res_comp, L == 2.97, V == 0.25), aes(F, absPAR, colour = model)) +
    geom_vline(xintercept = 1, linetype = "dashed") + geom_line(linewidth = 0.8) +
    lab_par + scale_models + x_ratio + scale_y_continuous(limits = c(0, 6)) + theme_canopy,
  "individual-competition", 9, 5)

## Fig. self-shading (a): focal LA 0.1 vs 40; SI: all focal LA
fla_lab <- as_labeller(function(x) paste("Focal LA =", x, "m²"))
save_fig(
  ggplot(filter(res_ss, fla %in% c(0.1, 40)), aes(F, absPAR, colour = model)) + geom_line() +
    facet_wrap(~fla, nrow = 1, labeller = fla_lab) + labs(tag = "(a)") +
    lab_par + scale_models + x_ratio + theme_canopy,
  "selfshading_c_fla_0.1_40", 9, 5)
save_fig(
  ggplot(res_ss, aes(F, absPAR, colour = model)) + geom_line() +
    facet_wrap(~fla, nrow = 2, labeller = fla_lab) +
    lab_par + scale_models + x_ratio + theme_canopy,
  "selfshading_c_fla", 10, 7)

## Fig. self-shading (b): Deep Crown minus MAESPA, tall focal tree
diff_df <- res_ss %>% filter(F == 1.99, model %in% c("DEEP CROWN", "MAESPA")) %>%
  select(fla, model, absPAR) %>% pivot_wider(names_from = model, values_from = absPAR) %>%
  mutate(diff = `DEEP CROWN` - MAESPA)
write_csv(diff_df, "selfshading2026_deepcrown_diff.csv")
save_fig(
  ggplot(diff_df, aes(fla, diff)) + geom_line(colour = "darkgreen") + geom_point(colour = "darkgreen") +
    labs(x = "\nFocal tree LA (m²)", y = "Deep Crown minus MAESPA,\nabsorbed PAR (MJ m-2 d-1)\n", tag = "(b)") +
    theme_canopy + theme(panel.border = element_blank(), axis.line = element_line(colour = "black")),
  "selfshading_deepcrown_diff", 7, 5)

## SI: single- vs two-stream radiative transfer (Deep Crown, LAI 2.97, CV 0.1)
one <- approx_par_2026(filter(lai_comp, model == "DEEP CROWN"), light1, G, stream = 1) %>%
  mutate(scheme = "Single-component")
two <- approx_par_2026(filter(lai_comp, model == "DEEP CROWN"), light, G, stream = 2) %>%
  mutate(scheme = "Two-component")
streams <- bind_rows(one, two) %>% filter(L == 2.97, V == 0.1)
save_fig(
  ggplot(streams, aes(F, absPAR)) +
    geom_line(aes(colour = "Deep Crown", linetype = scheme)) +
    geom_line(data = filter(m_comp, L == 2.97, V == 0.1), aes(colour = "MAESPA")) +
    scale_colour_manual(values = c("Deep Crown" = "goldenrod2", MAESPA = "grey45"), name = NULL) +
    scale_linetype_manual(values = c("Single-component" = 2, "Two-component" = 1), name = "Light model") +
    lab_par + labs(colour = NULL) + x_ratio + theme_canopy,
  "streams_deepcrown", 9, 5)

message("done")
