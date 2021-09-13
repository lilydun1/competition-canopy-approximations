library(Maeswrap)
library(tidyverse)
library(ggplot2)

#fla 0.1
H <- c(15)
V <- c(0.00, 0.10, 0.25, 0.50)
L <- c(0.466, 1.488, 2.916, 4.402, 5.485)
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
  add_column(model = "FT")

final_results_ppa_fla_0.1 <- organising_results(results_PAR_ppa_fla_0.1, combinations_fla_0.1) %>% 
  add_column(model = "PPA")

final_results_fla_0.1 <- rbind(final_results_ft_fla_0.1, final_results_ppa_fla_0.1) %>% 
  mutate(
    absPAR_one_s =  absPAR_one_s*fla, 
    absPAR_two_s = absPAR_two_s*fla
  )

maespa_fla_0.1 <- read_csv("maespa_fla_0.1.csv") 
maespa_fla_0.1 <- maespa_fla_0.1 %>% 
  mutate(
    absPAR_two_s = absPAR,
    absPAR_one_s = absPAR
  ) %>% 
  select(H, V, L, F, name, absPAR_one_s, absPAR_two_s) %>% 
  add_column(model = "maespa")

maespa_n_others_fla_0.1 <- rbind(final_results_fla_0.1, maespa_fla_0.1)

#fla 47.62 
H <- c(15)
V <- c(0.00, 0.10, 0.25, 0.50)
L <- c(0.466, 1.488, 2.916, 4.402, 5.485)
F <- c(1.99, 1.85, 1.75, 1.60, 1.50, 1.35, 1.25, 1.15, 1.05, 1.00, 
       0.95, 0.85, 0.75, 0.65, 0.50, 0.40, 0.25, 0.15, 0.01)
fla <- c(47.62)
S <- c(1, 2, 3)

combinations_fla_47.62 <- expand_grid(H, V, L, F, fla, S) %>% 
  mutate(path = sprintf("simulations_fla_47.62/H%s_V%s_L%s_F%s_fla%s_S%s", H, V, L, F, fla, S)) %>% 
  add_column(name = basename(.$path))

results_fla_47.62 <- combinations_fla_47.62$path[1:nrow(combinations_fla_47.62)] %>% 
  purrr::map(load_trees) 
names(results_fla_47.62) <- combinations_fla_47.62$path[1:nrow(combinations_fla_47.62)] %>% 
  basename()

model_conditions_fla_47.62 <- results_fla_47.62 %>% 
  purrr::map(model_simp)

results_PAR_ft_fla_47.62 <- model_conditions_fla_47.62 %>% 
  purrr::map(PAR_calculator_ft)

results_PAR_ppa_fla_47.62 <- model_conditions_fla_47.62 %>% 
  purrr::map(PAR_calculator_ppa)

final_results_ft_fla_47.62 <- organising_results(results_PAR_ft_fla_47.62, combinations_fla_47.62) %>% 
  add_column(model = "FT") 

final_results_ppa_fla_47.62 <- organising_results(results_PAR_ppa_fla_47.62, combinations_fla_47.62) %>% 
  add_column(model = "PPA") 

final_results_fla_47.62 <- rbind(final_results_ft_fla_47.62, final_results_ppa_fla_47.62) %>%  
  mutate(
  absPAR = absPAR*fla
)

maespa_fla_47.62 <- read_csv("maespa_fla_47.62.csv") 
maespa_fla_47.62 <- maespa_fla_47.62 %>% 
  select(H, V, L, F, name, absPAR) %>% 
  add_column(model = "maespa")

maespa_n_others_fla_47.62 <- rbind(final_results, maespa_fla_47.62)

#setting up with different focal las
H <- c(15)
V <- c(0.10)
L <- c(2.916)
F <- c(1.99, 1.85, 1.75, 1.60, 1.50, 1.35, 1.25, 1.15, 1.05, 1.00, 
       0.95, 0.85, 0.75, 0.65, 0.50, 0.40, 0.25, 0.15, 0.01)
fla <- c(0.1, 0.5, 1, 10, 50)
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
  add_column(model = "FT")

final_results_ppa_c_fla <- organising_results(results_PAR_ppa_c_fla, combinations_c_fla) %>% 
  add_column(model = "PPA")

final_results_c_fla <- rbind(final_results_ft_c_fla, final_results_ppa_c_fla) %>% 
  mutate(
    absPAR = absPAR*fla
  )

maespa_c_fla <- read_csv("maespa_c_fla.csv") 
maespa_c_fla <- maespa_c_fla %>% 
  select(H, V, L, F, fla, name, absPAR) %>% 
  add_column(model = "maespa")

maespa_n_others_c_fla <- rbind(final_results_c_fla, maespa_c_fla)


hour <- readhrflux(filename = "hrflux.dat")


