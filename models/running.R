library(Maeswrap)
library(tidyverse)
library(ggplot2)
library(purrr)

source("Function_M.R")

options(dplyr.summarise.inform = FALSE)

# fla 0.1
maespa_fla_0.1 <- load_maespa_results("Different maespa csv/maespa_fla_0.1.csv", 
                                      per_leaf_area = TRUE)

results_fla_0.1 <- 
  process_experiment("simulations_fla_0.1", 
                     H = c(15),
                     V = c(0, 0.1, 0.25, 0.5),
                     L = c(0.44, 1.521, 2.916, 4.556, 5.402),
                     F = c(1.99, 1.85, 1.75, 1.60, 1.50, 1.35, 1.25, 1.15, 1.05, 1.00, 
                           0.95, 0.85, 0.75, 0.65, 0.50, 0.40, 0.25, 0.15, 0.01),
                     fla = c(0.1),
                     S = c(1, 2, 3),
                     max_n = 100
                     )

results_fla_0.1_mn <- calculate_averages(results_fla_0.1)

final_results_fla_0.1 <- 
  bind_rows(results_fla_0.1_mn, maespa_fla_0.1) %>%
  arrange(name, model)

#stand 
maespa_stand_fla_0.1 <- load_maespa_results("Different maespa csv/maespa_stand_fla_0.1.csv", 
                                            per_leaf_area = FALSE)

results_stand_fla_0.1 <- 
  process_experiment("simulations_stand_fla_0.1", 
                     H = c(15),
                     V = c(0, 0.1, 0.25, 0.5),
                     L = c(0.44, 1.521, 2.916, 4.556, 5.402),
                     F = c(1.0),
                     fla = c(0.1),
                     S = c(1, 2, 3),
                     stand = TRUE,
                     max_n = 10
  )

results_stand_fla_0.1_mn <- calculate_averages(results_stand_fla_0.1, per_leaf_area = FALSE)

final_results_stand_fla_0.1 <- 
  bind_rows(results_stand_fla_0.1_mn, maespa_stand_fla_0.1) %>%
  arrange(name, model)


#setting up with different focal las
maespa_c_fla <- load_maespa_results("Different maespa csv/maespa_c_fla.csv", 
                                    per_leaf_area = TRUE)
  
results_c_fla <- 
  process_experiment("simulations_c_fla", 
                     H = c(15),
                     V = c(0.1),
                     L = c(2.916),
                     F = c(1.99, 1.85, 1.75, 1.60, 1.50, 1.35, 1.25, 1.15, 1.05, 1.00, 
                            0.95, 0.85, 0.75, 0.65, 0.50, 0.40, 0.25, 0.15, 0.01),
                     fla = c(0.1, 0.5, 1, 10, 20, 40),
                     S = c(1, 2, 3)
  )

results_c_fla_mn <- calculate_averages(results_c_fla)

final_results_c_fla <- 
  bind_rows(results_c_fla_mn, maespa_c_fla) %>%
  arrange(name, model)


#fla 27 
maespa_fla_27 <- load_maespa_results("Different maespa csv/maespa_fla_27.csv")

results_fla_27 <- 
  process_experiment("simulations_fla_27", 
                     H = c(15),
                     V = c(0, 0.1, 0.25, 0.5),
                     L = c(0.44, 1.521, 2.916, 4.556, 5.402),
                     F = c(1.99, 1.85, 1.75, 1.60, 1.50, 1.35, 1.25, 1.15, 1.05, 1.00, 
                           0.95, 0.85, 0.75, 0.65, 0.50, 0.40, 0.25, 0.15, 0.01),
                     fla = c(27),
                     S = c(1, 2, 3)
  )

results_fla_27_mn <- calculate_averages(results_fla_27)

final_results_fla_27 <- 
  bind_rows(results_fla_27_mn, maespa_fla_27) %>%
  arrange(name, model)
