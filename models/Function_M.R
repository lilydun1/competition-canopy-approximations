library(Maeswrap)
library(tidyverse)
library(ggplot2)

H <- c(15)
V <- c(0.00, 0.10, 0.25, 0.50)
L <- c(0.466, 1.488, 2.916, 4.402, 5.485)
F <- c(1.99, 1.85, 1.75, 1.60, 1.50, 1.35, 1.25, 1.15, 1.05, 1.00, 
       0.95, 0.85, 0.75, 0.65, 0.50, 0.40, 0.25, 0.15, 0.01)
S <- c(1, 2, 3)

combinations <- expand_grid(H, V, L, F, S) %>% 
  mutate(path = sprintf("simulations/H%s_V%s_L%s_F%s_S%s", H, V, L, F, S))
combinations_basenames <- combinations$path %>% 
  basename()
combinations <- combinations %>% 
  add_column(name = combinations_basenames)

load_trees <- function(path) {
  trees <- parseFile(file.path(path,"trees.dat"))
  if(trees$plot$notrees == 196) {
      data <- tibble(radx = trees$indivradx$values, rady =  trees$indivrady$values,  htcrown = trees$indivhtcrown$values, 
           diam = trees$indivdiam$values,  httrunk = trees$indivhttrunk$values, larea = trees$indivlarea$values, 
           x = trees$xy$xycoords[seq(1, length(trees$xy$xycoords), by = 2)], 
           y = trees$xy$xycoords[seq(2, length(trees$xy$xycoords), by = 2)],
           focal = ifelse(x == 105.54 & y == 90.46, TRUE, FALSE))
    } else if(trees$plot$notrees != 2304 & trees$plot$notrees != 196) {
    data <- tibble(radx = trees$indivradx$values, rady =  trees$indivrady$values,  htcrown = trees$indivhtcrown$values, 
                     diam = trees$indivdiam$values,  httrunk = trees$indivhttrunk$values, larea = trees$indivlarea$values, 
                     x = trees$xy$xycoords[seq(1, length(trees$xy$xycoords), by = 2)], 
                     y = trees$xy$xycoords[seq(2, length(trees$xy$xycoords), by = 2)],
                   focal = ifelse(x == 98 & y == 98, TRUE, FALSE))
    } else if(trees$plot$notrees == 2304) {
    data <- tibble(radx = trees$indivradx$values, rady =  trees$indivrady$values,  htcrown = trees$indivhtcrown$values, 
                     diam = trees$indivdiam$values,  httrunk = trees$indivhttrunk$values, larea = trees$indivlarea$values, 
                     x = trees$xy$xycoords[seq(1, length(trees$xy$xycoords), by = 2)], 
                     y = trees$xy$xycoords[seq(2, length(trees$xy$xycoords), by = 2)],
                   focal = ifelse(x == 100.09 & y == 95.91, TRUE, FALSE))
    }
  data <- data %>% 
    filter(x > 27.999, x < 168.001, y > 27.999, y < 168.001)
}

results <- combinations$path[1:nrow(combinations)] %>% 
  purrr::map(load_trees) 

names(results) <- combinations$path[1:nrow(combinations)] %>% 
  basename()

model_simp_f <- function(data) {
  df <- data %>% 
    mutate(t_h = htcrown + httrunk) %>% 
    arrange(desc(t_h)) %>% 
    group_by(t_h, focal) %>% 
    summarise(larea = sum(larea)) %>% 
    ungroup() %>% 
    arrange(desc(t_h)) %>% 
    mutate(
      lai = cumsum(larea)/10204, 
      light_ft = exp(-0.5*lai),
      lai_ppa = floor(lai),
      light_ppa = exp(-0.5*lai_ppa)) %>% 
    as_tibble()
}

model_simp_nf <- function(data) {
  df <- data %>% 
    mutate(t_h = htcrown + httrunk) %>% 
    arrange(desc(t_h)) %>% 
    group_by(t_h, focal) %>% 
    summarise(larea = sum(larea)) %>% 
    ungroup() %>% 
    arrange(desc(t_h)) %>% 
    mutate(
      lai = cumsum(larea)/10204, 
      light_ft = exp(-0.5*lai),
      lai_ppa = floor(lai),
      light_ppa = exp(-0.5*lai_ppa)) %>% 
    filter(focal == "FALSE")
}

plotting <- function(d_nf, d_f) {
  df_nf <- 
    bind_rows(d_nf, d_nf %>% mutate(t_h = lead(t_h), new = "new")) %>% 
    arrange(desc(t_h)) %>%
    replace_na(list(new = "og"))
  df_f <- 
    bind_rows(d_f, d_f %>% mutate(t_h = lead(t_h), new = "new")) %>% 
    arrange(desc(t_h)) %>%
    replace_na(list(new = "og")) %>% 
    filter(focal == "TRUE", new == "og")
  ggplot(data = df_nf, aes(t_h, light_ft)) +
    geom_line(colour='green') +
    geom_line(aes(t_h, light_ppa), colour='blue') +
    geom_point(data = df_f, aes(t_h, light_ft), colour='red') +
    geom_point(data = df_f, aes(t_h, light_ppa), colour='orange')
}
plotting(data_nf, data_f)

results_nf <- results %>% 
  purrr::map(model_simp_nf)

results_f <- results %>% 
  purrr::map(model_simp_f)

data_nf <- results_nf[["H15_V0.5_L1.488_F0.01_S2"]]

data_f <- results_f[["H15_V0.5_L5.485_F0.15_S1"]]

PAR_calculator <- function(data, indi_la = 47.62) {
  met <- readmet(filename = "met.dat", nlines = -1)
  la = indi_la
  L_ft = data %>% 
    filter(focal == "TRUE") %>% 
    pull(lai) 
  PAR_calculator <- 
    tibble(
      PAR_inst = met$PAR, 
      time = met$TIME) %>% 
    mutate(
      delta_t = c(diff(time[1:2]), diff(time)),
      absPAR_inst = PAR_inst*exp(-0.5*L_ft),#mu PAR m-2 s-1
      W = absPAR_inst/2.02, #W PAR m-2 eqn 38 from paper 
      MJ = W*0.0036 #MJ PAR m-2 h-1 eqn 12 from paper
    ) %>% 
    summarise(tree = (sum(MJ*delta_t)*la))
}

results_PAR <- results_f %>% 
  purrr::map(PAR_calculator)

combining_results <- function(sim_name) {
  combined_results <- combinations %>% 
    filter(name == sim_name) %>% 
    bind_cols(results_PAR[[sim_name]])
}

combined_results <- combinations$name[1:nrow(combinations)] %>% 
  purrr::map(combining_results) 

names(combined_results) <- combinations$name[1:nrow(combinations)] %>% 
  basename()

organising_results <- function(data) {
  df <- as.data.frame(matrix(unlist(data), nrow=length(unlist(data[1])))) 
  row.names(df) <- c("H", "V", "L", "F", "S", "path", "name", "tree")
  df <- t(df)
  row.names(df) <- 1:nrow(combinations)
  df <- df %>% as_tibble() %>% 
    select(H, V, L, F, S, path, tree) %>% 
    mutate(
      name = path %>% basename() %>% gsub("_S[1-3]", "",., perl = TRUE)
    )
  df <-  df %>% 
    transform(tree = as.numeric(tree), F = as.numeric(F), 
                  H = as.numeric(H), V = as.numeric(V),
                  L = as.numeric(L), S = as.numeric(S))
  df <- df %>% 
    group_by(H, V, L, F, name) %>% 
    summarise_at(vars(tree), mean)
}

d <- organising_results(combined_results)

names <- as_labeller(
  c(`0` = "V 0", `0.1` = "V 0.1", `0.25` = "V 0.25", `0.5` = "V 0.5", `0.75` = "V 0.75", 
    `0.466` = "LAI 0.467", `1.488`= "LAI 1.349", `2.916`= "LAI 2.917", `4.402`= "LAI 4.485", `5.485`= "LAI 5.395")
)

d %>% 
  select(F, tree, H, V, L) %>%
  ggplot(aes(F, tree)) + 
  geom_point(aes(colour = as.factor(H))) + 
  geom_line(aes(colour = as.factor(H))) + 
  facet_grid(rows = vars(V), cols = vars(L), labeller = names) +
  labs(title = "PAR", x = "FT height", y = "Absorbed PAR", colour = "Flat top")

