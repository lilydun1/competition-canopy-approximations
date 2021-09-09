library(Maeswrap)
library(tidyverse)
library(ggplot2)

H <- c(15)
V <- c(0.00, 0.10, 0.25, 0.50)
L <- c(0.466, 1.488, 2.916, 4.402, 5.485)
F <- c(1.99, 1.85, 1.75, 1.60, 1.50, 1.35, 1.25, 1.15, 1.05, 1.00, 
       0.95, 0.85, 0.75, 0.65, 0.50, 0.40, 0.25, 0.15, 0.01)
S <- c(1, 2, 3)

#setting up combinations of variables 
combinations <- expand_grid(H, V, L, F, S) %>% 
  mutate(path = sprintf("simulations/H%s_V%s_L%s_F%s_S%s", H, V, L, F, S)) %>% 
  add_column(name = basename(.$path))

#loading in all the trees file from MAESPA 
results <- combinations$path[1:nrow(combinations)] %>% 
  purrr::map(load_trees) 
names(results) <- combinations$path[1:nrow(combinations)] %>% 
  basename()

#setting up the lai and light conditions for ft and ppa
model_conditions <- results %>% 
  purrr::map(model_simp)

#working out the absPAR for the focal tree in each of the MAESPA simulations 
results_PAR_ft <- model_conditions %>% 
  purrr::map(PAR_calculator_ft)

results_PAR_ppa <- model_conditions %>% 
  purrr::map(PAR_calculator_ppa)

#turning the list of lists into a tibble, adding combination variables and averaging the three seeds 
final_results_ft <- organising_results(results_PAR_ft) %>% add_column(model = "FT")

final_results_ppa <- organising_results(results_PAR_ppa) %>% add_column(model = "PPA")

final_results <- rbind(final_results_ft, final_results_ppa)



maespa <- read_csv("A.csv") 
maespa <- maespa %>% 
  select( H, V, L, F, name, absPAR) %>% 
  add_column(model = "maespa")

trying <- rbind(new_final_results_ft, maespa)

trying %>% 
  select(F, absPAR, model, V, L) %>%
  ggplot(aes(F, absPAR)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_grid(rows = vars(V), cols = vars(L), labeller = names) +
  labs(x = "Focal tree height", y = "Absorbed PAR", colour = "Model")
