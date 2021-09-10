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
    absPAR = absPAR*fla
  )

maespa_fla_0.1 <- read_csv("maespa_fla_0.1.csv") 
maespa_fla_0.1 <- maespa_fla_0.1 %>% 
  select(H, V, L, F, name, absPAR) %>% 
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

combinations <- expand_grid(H, V, L, F, S) %>% 
  mutate(path = sprintf("simulations/H%s_V%s_L%s_F%s_S%s", H, V, L, F, S)) %>% 
  add_column(name = basename(.$path))

results <- combinations$path[1:nrow(combinations)] %>% 
  purrr::map(load_trees) 
names(results) <- combinations$path[1:nrow(combinations)] %>% 
  basename()

model_conditions <- results %>% 
  purrr::map(model_simp)

results_PAR_ft <- model_conditions %>% 
  purrr::map(PAR_calculator_ft)

results_PAR_ppa <- model_conditions %>% 
  purrr::map(PAR_calculator_ppa)

final_results_ft <- organising_results(results_PAR_ft, combinations) %>% 
  add_column(model = "FT")

final_results_ppa <- organising_results(results_PAR_ppa, combinations) %>% 
  add_column(model = "PPA")

final_results <- rbind(final_results_ft, final_results_ppa) #%>%  
  #mutate(
  #absPAR = absPAR*fla
#)

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

combinations_new <- expand_grid(H, V, L, F, fla, S) %>% 
  mutate(path = sprintf("simulations_new/H%s_V%s_L%s_F%s_fla%s_S%s", H, V, L, F, fla, S)) %>% 
  add_column(name = basename(.$path))

results_new <- combinations_new$path[1:nrow(combinations_new)] %>% 
  purrr::map(load_trees) 

names(results_new) <- combinations_new$path[1:nrow(combinations_new)] %>% 
  basename()

model_conditions_new <- results_new %>% 
  purrr::map(model_simp)

results_PAR_ft_new <- model_conditions_new %>% 
  purrr::map(PAR_calculator_ft)

results_PAR_ppa_new <- model_conditions_new %>% 
  purrr::map(PAR_calculator_ppa)

final_results_ft_new <- organising_results_new(results_PAR_ft_new, combinations_new) %>% 
  add_column(model = "FT")

final_results_ppa_new <- organising_results_new(results_PAR_ppa_new, combinations_new) %>% 
  add_column(model = "PPA")

final_results_new <- rbind(final_results_ft_new, final_results_ppa_new) %>% 
  mutate(
    absPAR = absPAR*fla
  )

maespa_c_fla <- read_csv("maespa_c_fla.csv") 
maespa_c_fla <- maespa_c_fla %>% 
  select(H, V, L, F, fla, name, absPAR) %>% 
  add_column(model = "maespa")

maespa_n_others_c_fla <- rbind(final_results_new, maespa_c_fla)


hour <- readhrflux(filename = "hrflux.dat")


