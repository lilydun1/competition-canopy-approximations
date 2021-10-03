library(Maeswrap)
library(tidyverse)
library(ggplot2)

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
  add_column(name = basename(.$path))

results_fla_0.1 <- combinations_fla_0.1$path[1:nrow(combinations_fla_0.1)] %>% 
  purrr::map(load_trees) 
names(results_fla_0.1) <- combinations_fla_0.1$path[1:nrow(combinations_fla_0.1)] %>% 
  basename()

model_conditions_fla_0.1 <- results_fla_0.1 %>% 
  purrr::map(model_simp)

results_PAR_ft_fla_0.1 <- model_conditions_fla_0.1 %>% 
  purrr::map(PAR_calculator_ft)

results_PAR_ppa_fla_0.1 <- model_conditions_fla_0.1 %>% 
  purrr::map(PAR_calculator_ppa)

final_results_ft_fla_0.1 <- organising_results(results_PAR_ft_fla_0.1, combinations_fla_0.1) %>% 
  add_column(model = "FLAT TOP")

final_results_ppa_fla_0.1 <- organising_results(results_PAR_ppa_fla_0.1, combinations_fla_0.1) %>% 
  add_column(model = "PPA")

final_results_ft_ppa_fla_0.1 <- rbind(final_results_ft_fla_0.1, final_results_ppa_fla_0.1) %>% 
  mutate(
    absPAR_one_s =  absPAR_one_s*fla, 
    absPAR_two_s = absPAR_two_s*fla
  )

deep_crown_fla_0.1 <- results_fla_0.1 %>% 
  purrr::map(deep_crown_set_up) 

slices_DC_absPAR_fla_0.1 <- deep_crown_fla_0.1 %>% 
  purrr::map(applying_DC) 

for(i in 1:nrow(combinations_fla_0.1)) {
  slices_DC_absPAR_fla_0.1[[i]] <- slices_DC_absPAR_fla_0.1[[i]] %>% 
    mutate(
      absPAR_one_s = absPAR_one_s*combinations_fla_0.1$fla[[i]],
      absPAR_two_s = absPAR_two_s*combinations_fla_0.1$fla[[i]])
}

summarised_DC_results_fla_0.1 <- 
  slices_DC_absPAR_fla_0.1 %>% 
  purrr::map(summarise_DC) 

final_results_DC_fla_0.1 <- organising_results(summarised_DC_results_fla_0.1, combinations_fla_0.1) %>% 
  add_column(model = "DEEP CROWN")

final_results_fla_0.1 <- rbind(final_results_DC_fla_0.1, final_results_ft_ppa_fla_0.1)

maespa_fla_0.1 <- read_csv("maespa_fla_0.1.csv") 
maespa_fla_0.1 <- maespa_fla_0.1 %>% 
  mutate(
    absPAR_one_s = absPAR, 
    absPAR_two_s = absPAR
  ) %>% 
  select(H, V, L, F, fla, name, absPAR_one_s, absPAR_two_s) %>% 
  add_column(model = "MAESPA")

maespa_n_others_fla_0.1 <- rbind(final_results_fla_0.1, maespa_fla_0.1)


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
  add_column(name = basename(.$path))

results_c_fla <- combinations_c_fla$path[1:nrow(combinations_c_fla)] %>% 
  purrr::map(load_trees) 
names(results_c_fla) <- combinations_c_fla$path[1:nrow(combinations_c_fla)] %>% 
  basename()

model_conditions_c_fla <- results_c_fla %>% 
  purrr::map(model_simp)

results_PAR_ft_c_fla <- model_conditions_c_fla %>% 
  purrr::map(PAR_calculator_ft)

results_PAR_ppa_c_fla <- model_conditions_c_fla %>% 
  purrr::map(PAR_calculator_ppa)

final_results_ft_c_fla <- organising_results(results_PAR_ft_c_fla, combinations_c_fla) %>% 
  add_column(model = "FLAT TOP")

final_results_ppa_c_fla <- organising_results(results_PAR_ppa_c_fla, combinations_c_fla) %>% 
  add_column(model = "PPA")

final_results_ft_ppa_c_fla <- rbind(final_results_ft_c_fla, final_results_ppa_c_fla) %>% 
  mutate(
    absPAR_one_s =  absPAR_one_s*fla, 
    absPAR_two_s = absPAR_two_s*fla
  )

deep_crown_c_fla <- results_c_fla %>% 
  purrr::map(deep_crown_set_up) 

slices_DC_absPAR_c_fla <- deep_crown_c_fla %>% 
  purrr::map(applying_DC) 

for(i in 1:nrow(combinations_c_fla)) {
  slices_DC_absPAR_c_fla[[i]] <- slices_DC_absPAR_c_fla[[i]] %>% 
    mutate(
      absPAR_one_s = absPAR_one_s*combinations_c_fla$fla[[i]],
      absPAR_two_s = absPAR_two_s*combinations_c_fla$fla[[i]])
}

summarised_DC_results_c_fla <- 
  slices_DC_absPAR_c_fla %>% 
  purrr::map(summarise_DC) 

final_results_DC_c_fla <- organising_results(summarised_DC_results_c_fla, combinations_c_fla) %>% 
  add_column(model = "DEEP CROWN")

final_results_c_fla <- rbind(final_results_DC_c_fla, final_results_ft_ppa_c_fla)

maespa_c_fla <- read_csv("maespa_c_fla.csv") 
maespa_c_fla <- maespa_c_fla %>% 
  mutate(
    absPAR_two_s = absPAR,
    absPAR_one_s = absPAR) %>% 
  select(H, V, L, F, fla, name, absPAR_one_s, absPAR_two_s) %>% 
  add_column(model = "MAESPA")

maespa_n_others_c_fla <- rbind(final_results_c_fla, maespa_c_fla)

#stand 
H <- c(15)
V <- c(0, 0.1, 0.25, 0.5)
L <- c(0.44, 1.521, 2.916, 4.556, 5.402)
F <- c(1.00)
fla <- c(0.1)
S <- c(1, 2, 3)

combinations_stand_fla_0.1 <- expand_grid(H, V, L, F, fla, S) %>% 
  mutate(path = sprintf("simulations_stand_fla_0.1/H%s_V%s_L%s_F%s_fla%s_S%s", H, V, L, F, fla, S)) %>% 
  add_column(name = basename(.$path))

results_stand_fla_0.1 <- combinations_stand_fla_0.1$path[1:nrow(combinations_stand_fla_0.1)] %>% 
  purrr::map(load_trees_stand) 
names(results_stand_fla_0.1) <- combinations_stand_fla_0.1$path[1:nrow(combinations_stand_fla_0.1)] %>% 
  basename()

model_conditions_stand_fla_0.1 <- results_stand_fla_0.1 %>% 
  purrr::map(model_simp)

results_PAR_ft_stand_fla_0.1 <- model_conditions_stand_fla_0.1 %>% 
  purrr::map(PAR_calculator_ft_stand)

results_PAR_ppa_stand_fla_0.1 <- model_conditions_stand_fla_0.1 %>% 
  purrr::map(PAR_calculator_ppa_stand)

final_results_ft_stand_fla_0.1 <- organising_results_stand(results_PAR_ft_stand_fla_0.1, combinations_stand_fla_0.1) %>% 
  add_column(model = "FLAT TOP")

final_results_ppa_stand_fla_0.1 <- organising_results_stand(results_PAR_ppa_stand_fla_0.1, combinations_stand_fla_0.1) %>% 
  add_column(model = "PPA")

final_results_ft_ppa_stand_fla_0.1 <- rbind(final_results_ft_stand_fla_0.1, final_results_ppa_stand_fla_0.1)

deep_crown_stand_fla_0.1 <- results_stand_fla_0.1 %>% 
  purrr::map(deep_crown_set_up_stand) 

slices_DC_absPAR_stand_fla_0.1 <- deep_crown_stand_fla_0.1 %>% 
  purrr::map(applying_DC_stand) 

summarised_DC_results_stand_fla_0.1 <- 
  slices_DC_absPAR_stand_fla_0.1 %>% 
  purrr::map(summarise_DC_stand)

final_results_DC_stand_fla_0.1 <- organising_results_stand(summarised_DC_results_stand_fla_0.1, combinations_stand_fla_0.1) %>% 
  add_column(model = "DEEP CROWN")

final_results_stand_fla_0.1 <- rbind(final_results_DC_stand_fla_0.1, final_results_ft_ppa_stand_fla_0.1)

maespa_stand_fla_0.1 <- read_csv("maespa_stand_fla_0.1.csv") 

maespa_stand_fla_0.1 <- maespa_stand_fla_0.1 %>% 
  mutate(
    absPAR_two_s = absPAR,
    absPAR_one_s = absPAR) %>% 
  select(H, V, L, F, fla, name, absPAR_one_s, absPAR_two_s) %>% 
  add_column(model = "MAESPA") 

maespa_stand_fla_0.1 <- maespa_stand_fla_0.1 %>% 
  group_by(H, V, L, F, fla, name, model) %>% 
  summarise_at(vars(absPAR_one_s, absPAR_two_s), sum)

maespa_n_others_stand_fla_0.1 <- rbind(final_results_stand_fla_0.1, maespa_stand_fla_0.1)

