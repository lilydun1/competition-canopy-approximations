library(Maeswrap)
library(tidyverse)
library(ggplot2)
library(purrr)

#fla 0.1
H <- c(15)
V <- c(0, 0.1, 0.25, 0.5)
L <- c(0.44, 1.521, 2.916, 4.556, 5.402)
F <- c(1.99, 1.85, 1.75, 1.60, 1.50, 1.35, 1.25, 1.15, 1.05, 1.00, 
       0.95, 0.85, 0.75, 0.65, 0.50, 0.40, 0.25, 0.15, 0.01)
fla <- c(0.1)
S <- c(1, 2, 3)

combinations_fla_0.1 <- expand_grid(H, V, L, F, fla, S) %>% 
  mutate(path = sprintf("simulations_fla_0.1/H%s_V%s_L%s_F%s_fla%s_S%s", H, V, L, F, fla, S)) %>% 
  add_column(name = basename(.$path)) %>% 
  mutate(trees = map(path, load_trees), 
         f_trees = map(trees, focal_tree))

results_fla_0.1 <- 
  combinations_fla_0.1 %>%
  mutate(
  model_conditions = map(f_trees, model_simp), 
  ppa = map(model_conditions, PAR_calculator_ppa), 
  ft = map(model_conditions, PAR_calculator_ft), 
  dc_slices = map(f_trees, deep_crown_set_up), 
  dc = map(dc_slices, applying_DC), 
  dc_m = map(dc, summarise_DC))

final_fla_0.1 <- results_fla_0.1 %>% 
  select(H, V, L, F, fla, S, name)

PPA_fla_0.1 <- organising_results(results_fla_0.1$ppa, final_fla_0.1) %>% 
  add_column(model = "PPA") 
FT_fla_0.1 <- organising_results(results_fla_0.1$ft, final_fla_0.1) %>% 
  add_column(model = "FLAT TOP")
DC_fla_0.1 <- organising_results(results_fla_0.1$dc_m, final_fla_0.1) %>% 
  add_column(model = "DEEP CROWN")

maespa_fla_0.1 <- read_csv("maespa_fla_0.1.csv") 
maespa_fla_0.1 <- maespa_fla_0.1 %>% 
  mutate(
    absPAR_one_s = absPAR/0.1, 
    absPAR_two_s = absPAR/0.1
  ) %>% 
  select(H, V, L, F, fla, name, absPAR_one_s, absPAR_two_s) %>% 
  add_column(model = "MAESPA")

final_results_fla_0.1 <- rbind(PPA_fla_0.1, FT_fla_0.1, DC_fla_0.1, maespa_fla_0.1)

#stand 
H <- c(15)
V <- c(0, 0.1, 0.25, 0.5)
L <- c(0.44, 1.521, 2.916, 4.556, 5.402)
F <- c(1.00)
fla <- c(0.1)
S <- c(1, 2, 3)

combinations_stand_fla_0.1 <- expand_grid(H, V, L, F, fla, S) %>% 
  mutate(path = sprintf("simulations_stand_fla_0.1/H%s_V%s_L%s_F%s_fla%s_S%s", H, V, L, F, fla, S)) %>% 
  add_column(name = basename(.$path)) %>% 
  mutate(trees = map(path, load_trees), 
         f_trees = map(trees, focal_tree_stand))

results_stand_fla_0.1 <- 
  combinations_stand_fla_0.1 %>%
  mutate(
    model_conditions = map(f_trees, model_simp), 
    ppa = map(model_conditions, PAR_calculator_ft_stand), 
    ft = map(model_conditions, PAR_calculator_ppa_stand), 
    dc_slices = map(f_trees, deep_crown_set_up_stand), 
    dc = map(dc_slices, applying_DC_stand), 
    dc_m = map(dc, summarise_DC_stand)
)

final_stand_fla_0.1 <- results_fla_0.1 %>% 
  select(H, V, L, F, fla, S, name)

PPA_stand_fla_0.1 <- organising_results(results_stand_fla_0.1$ppa, final_stand_fla_0.1) %>% 
  add_column(model = "PPA") 
FT_stand_fla_0.1 <- organising_results(results_stand_fla_0.1$ft, final_stand_fla_0.1) %>% 
  add_column(model = "FLAT TOP")
DC_stand_fla_0.1 <- organising_results(results_stand_fla_0.1$dc_m, final_stand_fla_0.1) %>% 
  add_column(model = "DEEP CROWN")

maespa_stand_fla_0.1 <- read_csv("maespa_stand_fla_0.1.csv") 
maespa_stand_fla_0.1 <- maespa_stand_fla_0.1 %>% 
  mutate(
    absPAR_two_s = absPAR,
    absPAR_one_s = absPAR) %>% 
  select(H, V, L, F, fla, name, absPAR_one_s, absPAR_two_s) %>% 
  add_column(model = "MAESPA") 

final_results_stand_fla_0.1 <- rbind(PPA_stand_fla_0.1, FT_stand_fla_0.1, DC_stand_fla_0.1, maespa_stand_fla_0.1)
final_results_stand_fla_0.1 <- final_results_stand_fla_0.1 %>% 
  mutate(absPAR_one_s = absPAR_one_s/0.1,
         absPAR_two_s = absPAR_two_s/0.1)

#setting up with different focal las
H <- c(15)
V <- c(0.1)
L <- c(2.916)
F <- c(1.99, 1.85, 1.75, 1.60, 1.50, 1.35, 1.25, 1.15, 1.05, 1.00, 
       0.95, 0.85, 0.75, 0.65, 0.50, 0.40, 0.25, 0.15, 0.01)
fla <- c(0.1, 0.5, 1, 10, 20, 40)
S <- c(1, 2, 3)

combinations_c_fla <- expand_grid(H, V, L, F, fla, S) %>% 
  mutate(path = sprintf("simulations_c_fla/H%s_V%s_L%s_F%s_fla%s_S%s", H, V, L, F, fla, S)) %>% 
  add_column(name = basename(.$path)) %>% 
  mutate(trees = map(path, load_trees), 
         f_trees = map(trees, focal_tree))

results_c_fla <- 
  combinations_c_fla %>%
  mutate(
    model_conditions = map(f_trees, model_simp), 
    ppa = map(model_conditions, PAR_calculator_ppa), 
    ft = map(model_conditions, PAR_calculator_ft), 
    dc_slices = map(f_trees, deep_crown_set_up), 
    dc = map(dc_slices, applying_DC), 
    dc_m = map(dc, summarise_DC))

final_c_fla <- results_c_fla %>% 
  select(H, V, L, F, fla, S, name)

PPA_c_fla <- organising_results(results_c_fla$ppa, final_c_fla) %>% 
  add_column(model = "PPA") 
FT_c_fla <- organising_results(results_c_fla$ft, final_c_fla) %>% 
  add_column(model = "FLAT TOP")
DC_c_fla <- organising_results(results_c_fla$dc_m, final_c_fla) %>% 
  add_column(model = "DEEP CROWN")

maespa_c_fla <- read_csv("maespa_c_fla.csv") 
maespa_c_fla <- maespa_c_fla %>% 
  mutate(
    absPAR_two_s = absPAR/fla,
    absPAR_one_s = absPAR/fla) %>% 
  select(H, V, L, F, fla, name, absPAR_one_s, absPAR_two_s) %>% 
  add_column(model = "MAESPA")

final_results_c_fla <- rbind(PPA_c_fla, FT_c_fla, DC_c_fla, maespa_c_fla)

#fla 27 
H <- c(15)
V <- c(0, 0.1, 0.25, 0.5)
L <- c(0.44, 1.521, 2.916, 4.556, 5.402)
F <- c(1.99, 1.85, 1.75, 1.60, 1.50, 1.35, 1.25, 1.15, 1.05, 1.00, 
       0.95, 0.85, 0.75, 0.65, 0.50, 0.40, 0.25, 0.15, 0.01)
fla <- c(27)
S <- c(1, 2, 3)

combinations_fla_27 <- expand_grid(H, V, L, F, fla, S) %>% 
  mutate(path = sprintf("simulations_fla_27/H%s_V%s_L%s_F%s_fla%s_S%s", H, V, L, F, fla, S)) %>% 
  add_column(name = basename(.$path))

results_fla_27 <- combinations_fla_27$path[1:nrow(combinations_fla_27)] %>% 
  purrr::map(load_trees) 
names(results_fla_27) <- combinations_fla_27$path[1:nrow(combinations_fla_27)] %>% 
  basename()

model_conditions_fla_27 <- results_fla_27 %>% 
  purrr::map(model_simp)

results_PAR_ft_fla_27 <- model_conditions_fla_27 %>% 
  purrr::map(PAR_calculator_ft)

results_PAR_ppa_fla_27 <- model_conditions_fla_27 %>% 
  purrr::map(PAR_calculator_ppa)

final_results_ft_fla_27 <- organising_results(results_PAR_ft_fla_27, combinations_fla_27) %>% 
  add_column(model = "FLAT TOP") 

final_results_ppa_fla_27 <- organising_results(results_PAR_ppa_fla_27, combinations_fla_27) %>% 
  add_column(model = "PPA") 

final_results_ft_ppa_fla_27 <- rbind(final_results_ft_fla_27, final_results_ppa_fla_27) %>%  
  mutate(
    absPAR_one_s =  absPAR_one_s*fla, 
    absPAR_two_s = absPAR_two_s*fla
  )

deep_crown_fla_27 <- results_fla_27 %>% 
  purrr::map(deep_crown_set_up) 

slices_DC_absPAR_fla_27 <- deep_crown_fla_27 %>% 
  purrr::map(applying_DC) 

for(i in 1:nrow(combinations_fla_27)) {
  slices_DC_absPAR_fla_27[[i]] <- slices_DC_absPAR_fla_27[[i]] %>% 
    mutate(
      absPAR_one_s = absPAR_one_s*combinations_fla_27$fla[[i]],
      absPAR_two_s = absPAR_two_s*combinations_fla_27$fla[[i]])
}

summarised_DC_results_fla_27 <- 
  slices_DC_absPAR_fla_27 %>% 
  purrr::map(summarise_DC) 

final_results_DC_fla_27 <- organising_results(summarised_DC_results_fla_27, combinations_fla_27) %>% 
  add_column(model = "DEEP CROWN")

final_results_fla_27 <- rbind(final_results_DC_fla_27, final_results_ft_ppa_fla_27)

maespa_fla_27 <- read_csv("maespa_fla_27.csv") 
maespa_fla_27 <- maespa_fla_27 %>% 
  mutate(
    absPAR_two_s = absPAR,
    absPAR_one_s = absPAR) %>% 
  select(H, V, L, F, fla, name, absPAR_one_s, absPAR_two_s) %>% 
  add_column(model = "MAESPA")

maespa_n_others_fla_27 <- rbind(final_results_fla_27, maespa_fla_27)
