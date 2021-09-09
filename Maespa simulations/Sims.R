library(Maeswrap)
library(rgl)
library(tidyverse)

source("r/Function.R")

#variation in the focal tree LAI 
H <- c(15)
V <- c(0.10)
L <- c(2.916)
F <- c(1.99, 1.85, 1.75, 1.60, 1.50, 1.35, 1.25, 1.15, 1.05, 1.00, 
       0.95, 0.85, 0.75, 0.65, 0.50, 0.40, 0.25, 0.15, 0.01)
fla <- c(0.1, 0.5, 1, 10, 50)
S <- c(1, 2, 3)

combinations_new <- expand_grid(H, V, L, F, fla, S) %>% 
  mutate(path = sprintf("simulations_new/H%s_V%s_L%s_F%s_fla%s_S%s", H, V, L, F, fla, S))

for(i in 1:nrow(combinations_new)) {
  create_simulation(path = combinations_new$path[i], template = "template_A", 
                    h_mn = combinations_new$H[i], h_cv = combinations_new$V[i], 
                    LAI = combinations_new$L[i], ft_h = combinations_new$F[i], 
                    fla = combinations_new$fla[i], seed = combinations_new$S[i])
}

for(i in 1:nrow(combinations_new)) {
  run_simulation(path = combinations_new$path[i])
}

output_new <- combinations_new$path %>% 
  purrr:: map_df(load_output)

output_combined_new <- combinations_new %>% 
  left_join(output_new, by = "path")

mn_outputs_new <- output_combined_new %>% 
  group_by(H, V, L, F, fla, Tree, name) %>% 
  summarise_at(vars(absPAR, absNIR, absTherm, totPs, netPs, totRf, totLE1, totLE2, totH), mean) 


#for the average one with everything
H <- c(15)
V <- c(0.00, 0.10, 0.25, 0.50)
L <- c(0.466, 1.488, 2.916, 4.402, 5.485)
F <- c(1.99, 1.85, 1.75, 1.60, 1.50, 1.35, 1.25, 1.15, 1.05, 1.00, 
       0.95, 0.85, 0.75, 0.65, 0.50, 0.40, 0.25, 0.15, 0.01)
S <- c(1, 2, 3)

combinations_A <- expand_grid(H, V, L, F, S) %>% 
  mutate(path = sprintf("simulations_A/H%s_V%s_L%s_F%s_S%s", H, V, L, F, S))

for(i in 1:nrow(combinations_A)) {
  create_simulation(path = combinations_A$path[i], template = "template_A", h_mn = combinations_A$H[i], h_cv = combinations_A$V[i], 
                    LAI = combinations_A$L[i], ft_h = combinations_A$F[i], seed = combinations_A$S[i])
}

for(i in 1:nrow(combinations_A)) {
  run_simulation(path = combinations_A$path[i])
}

output_A <- combinations_A$path %>% 
  purrr:: map_df(load_output)

output_combined_A <- combinations_A %>% 
  left_join(output_A, by = "path")

mn_outputs_A <- output_combined_A %>% 
  group_by(H, V, L, F, Tree, name) %>% 
  summarise_at(vars(absPAR, absNIR, absTherm, totPs, netPs, totRf, totLE1, totLE2, totH), mean) 

#wet and dry 
H <- c(15)
V <- c(0.10, 0.25)
L <- c(2.916, 4.402, 5.485)
F <- c(1.99, 1.50, 1.00, 0.50, 0.01)
WD <- c(0, 1, 2, 3, 4)
S <- c(1, 2, 3)

combinations_WD <- expand_grid(H, V, L, F, WD, S) %>% 
  mutate(path = sprintf("simulations_WD/H%s_V%s_L%s_F%s_WD%s_S%s", H, V, L, F, WD, S))

combinations_WD0 <- combinations_WD %>% 
  filter(WD == "0")

for(i in 1:nrow(combinations_WD0)) {
  create_simulation(path = combinations_WD0$path[i], template = "template_WD0", h_mn = combinations_WD0$H[i], h_cv = combinations_WD0$V[i], 
                    LAI = combinations_WD0$L[i], ft_h = combinations_WD0$F[i])
}

for(i in 1:nrow(combinations_WD0)) {
  run_simulation(path = combinations_WD0$path[i])
}

combinations_WD1 <- combinations_WD %>% 
  filter(WD == "1")

for(i in 1:nrow(combinations_WD1)) {
  create_simulation(path = combinations_WD1$path[i], template = "template_WD1", h_mn = combinations_WD1$H[i], h_cv = combinations_WD1$V[i], 
                    LAI = combinations_WD1$L[i], ft_h = combinations_WD1$F[i])
}

for(i in 1:nrow(combinations_WD1)) {
  run_simulation(path = combinations_WD1$path[i])
}

combinations_WD2 <- combinations_WD %>% 
  filter(WD == "2")

for(i in 1:nrow(combinations_WD2)) {
  create_simulation(path = combinations_WD2$path[i], template = "template_WD2", h_mn = combinations_WD2$H[i], h_cv = combinations_WD2$V[i], 
                    LAI = combinations_WD2$L[i], ft_h = combinations_WD2$F[i])
}

for(i in 1:nrow(combinations_WD2)) {
  run_simulation(path = combinations_WD2$path[i])
}

combinations_WD3 <- combinations_WD %>% 
  filter(WD == "3")

for(i in 1:nrow(combinations_WD3)) {
  create_simulation(path = combinations_WD3$path[i], template = "template_WD3", h_mn = combinations_WD3$H[i], h_cv = combinations_WD3$V[i], 
                    LAI = combinations_WD3$L[i], ft_h = combinations_WD3$F[i])
}

for(i in 1:nrow(combinations_WD3)) {
  run_simulation(path = combinations_WD3$path[i])
}

combinations_WD4 <- combinations_WD %>% 
  filter(WD == "4")

for(i in 1:nrow(combinations_WD4)) {
  create_simulation(path = combinations_WD4$path[i], template = "template_WD4", h_mn = combinations_WD4$H[i], h_cv = combinations_WD4$V[i], 
                    LAI = combinations_WD4$L[i], ft_h = combinations_WD4$F[i])
}

for(i in 1:nrow(combinations_WD4)) {
  run_simulation(path = combinations_WD4$path[i])
}

output_WD <- combinations_WD$path %>% 
  purrr:: map_df(load_output)

output_combined_WD <- combinations_WD %>% 
  left_join(output_WD, by = "path")

mn_outputs_WD <- output_combined_WD %>% 
  group_by(H, V, L, F, WD, Tree, name) %>% 
  summarise_at(vars(absPAR, absNIR, absTherm, totPs, netPs, totRf, totLE1, totLE2, totH), mean) 




