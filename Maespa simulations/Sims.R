library(Maeswrap)
library(rgl)
library(tidyverse)
library(purrr)

source("r/Function.R")

#maespa but with 0.1 la for focal tree 
H <- c(15)
V <- c(0, 0.1, 0.25, 0.5)
L <- c(0.44, 1.521, 2.916, 4.556, 5.402)
F <- c(1.99, 1.85, 1.75, 1.60, 1.50, 1.35, 1.25, 1.15, 1.05, 1.00, 
       0.95, 0.85, 0.75, 0.65, 0.50, 0.40, 0.25, 0.15, 0.01)
fla <- c(0.1)
S <- c(1, 2, 3)

combinations_fla_0.1 <- expand_grid(H, V, L, F, fla, S) %>% 
  mutate(path = sprintf("simulations_fla_0.1/H%s_V%s_L%s_F%s_fla%s_S%s", H, V, L, F, fla, S))

for(i in 1:nrow(combinations_fla_0.1)) {
  create_simulation(path = combinations_fla_0.1$path[i], template = "template_A", 
                    h_mn = combinations_fla_0.1$H[i], h_cv = combinations_fla_0.1$V[i], 
                    LAI = combinations_fla_0.1$L[i], ft_h = combinations_fla_0.1$F[i], 
                    fla = combinations_fla_0.1$fla[i], seed = combinations_fla_0.1$S[i])
}

for(i in 1:nrow(combinations_fla_0.1)) {
  run_simulation(path = combinations_fla_0.1$path[i])
}

output_fla_0.1 <- combinations_fla_0.1$path %>% 
  map_df(load_output)

output_combined_fla_0.1 <- combinations_fla_0.1 %>% 
  left_join(output_fla_0.1, by = "path")

mn_outputs_fla_0.1 <- output_combined_fla_0.1 %>% 
  group_by(H, V, L, F, fla, Tree, name) %>% 
  summarise_at(vars(absPAR, absNIR, absTherm, totPs, netPs, totRf, totLE1, totLE2, totH), mean)

#for the average one with everything and 27 la 
H <- c(15)
V <- c(0, 0.1, 0.25, 0.5)
L <- c(0.44, 1.521, 2.916, 4.556, 5.402)
F <- c(1.99, 1.85, 1.75, 1.60, 1.50, 1.35, 1.25, 1.15, 1.05, 1.00, 
       0.95, 0.85, 0.75, 0.65, 0.50, 0.40, 0.25, 0.15, 0.01)
fla <- c(27)
S <- c(1, 2, 3)

combinations_fla_27 <- expand_grid(H, V, L, F, fla, S) %>% 
  mutate(path = sprintf("simulations_fla_27/H%s_V%s_L%s_F%s_fla%s_S%s", H, V, L, F, fla, S))

for(i in 1:nrow(combinations_fla_27)) {
  create_simulation(path = combinations_fla_27$path[i], template = "template_A", 
                    h_mn = combinations_fla_27$H[i], h_cv = combinations_fla_27$V[i], 
                    LAI = combinations_fla_27$L[i], ft_h = combinations_fla_27$F[i], 
                    fla = combinations_fla_27$fla[i], seed = combinations_fla_27$S[i])
}

for(i in 1:nrow(combinations_fla_27)) {
  run_simulation(path = combinations_fla_27$path[i])
}

output_fla_27 <- combinations_fla_27$path %>% 
  map_df(load_output)

output_combined_fla_27 <- combinations_fla_27 %>% 
  left_join(output_fla_27, by = "path")

mn_outputs_fla_27 <- output_combined_fla_27 %>% 
  group_by(H, V, L, F, fla, Tree, name) %>% 
  summarise_at(vars(absPAR, absNIR, absTherm, totPs, netPs, totRf, totLE1, totLE2, totH), mean) 


#variation in the focal tree LAI 
H <- c(15)
V <- c(0.1)
L <- c(2.916)
F <- c(1.99, 1.85, 1.75, 1.60, 1.50, 1.35, 1.25, 1.15, 1.05, 1.00, 
       0.95, 0.85, 0.75, 0.65, 0.50, 0.40, 0.25, 0.15, 0.01)
fla <- c(0.1, 0.5, 1, 10, 20, 40)
S <- c(1, 2, 3)

combinations_c_fla <- expand_grid(H, V, L, F, fla, S) %>% 
  mutate(path = sprintf("simulations_c_fla/H%s_V%s_L%s_F%s_fla%s_S%s", H, V, L, F, fla, S))

for(i in 1:nrow(combinations_c_fla)) {
  create_simulation(path = combinations_c_fla$path[i], template = "template_A", 
                    h_mn = combinations_c_fla$H[i], h_cv = combinations_c_fla$V[i], 
                    LAI = combinations_c_fla$L[i], ft_h = combinations_c_fla$F[i], 
                    fla = combinations_c_fla$fla[i], seed = combinations_c_fla$S[i])
}

for(i in 1:nrow(combinations_c_fla)) {
  run_simulation(path = combinations_c_fla$path[i])
}

output_c_fla <- combinations_c_fla$path %>% 
  map_df(load_output)

output_combined_c_fla <- combinations_c_fla %>% 
  left_join(output_c_fla, by = "path")

mn_outputs_c_fla <- output_combined_c_fla %>% 
  group_by(H, V, L, F, fla, Tree, name) %>% 
  summarise_at(vars(absPAR, absNIR, absTherm, totPs, netPs, totRf, totLE1, totLE2, totH), mean)

#whole stand as focal trees
L <- c(0.44, 1.521, 2.916, 4.556, 5.402)

create_simulation_f_stand(path = "poop/poop", template = "template_A", 
                  h_mn = 15, h_cv = 0, 
                  LAI = 5.402, ft_h = 0.5, 
                  fla = 0.1, seed = 1)
run_simulation(path = "poop/poop")

Plotstand(treesfile = "trees.dat")

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




