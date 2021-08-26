library(Maeswrap)
library(tidyverse)

H <- c(15)
V <- c(0.00, 0.10, 0.25, 0.50)
L <- c(0.466, 1.488, 2.916, 4.402, 5.485)
F <- c(1.99, 1.85, 1.75, 1.60, 1.50, 1.35, 1.25, 1.15, 1.05, 1.00, 
       0.95, 0.85, 0.75, 0.65, 0.50, 0.40, 0.25, 0.15, 0.01)
S <- c(1, 2, 3)

combinations <- expand_grid(H, V, L, F, S) %>% 
  mutate(path = sprintf("simulations/H%s_V%s_L%s_F%s_S%s", H, V, L, F, S))

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
      light_ppa = exp(-0.5*lai_ppa))
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

data_f <- model_simp_f(results[["H15_V0.5_L4.402_F1.99_S1"]])

data_nf <- model_simp_nf(results[["H15_V0.5_L1.488_F1.99_S2"]])

plotting(data_nf, data_f)

#light interception
I_0 = 383.9562 
k = 0.5
L = data_f %>% 
  filter(focal == "TRUE") %>% 
  select(lai_ppa) %>% 
  as.numeric()

I_z = I_0*exp(-k*L)

outputs <- data.frame(absPAR = I_z, H = 15, V = 0.5, L = 4.402, F = 1.99, S = 1)

outputs %>% 
  ggplot(aes(F, absPAR)) + 
  geom_point(aes(colour = as.factor(H))) + 
  geom_line(aes(colour = as.factor(H))) + 
  facet_grid(rows = vars(V), cols = vars(L), labeller = names) +
  labs(title = "PAR", x = "FT height", y = "Absorbed PAR", colour = "Height")

# create data frame with absPAR as variable and all the different variables 
names <- as_labeller(
  c(`0` = "V 0", `0.1` = "V 0.1", `0.25` = "V 0.25", `0.5` = "V 0.5", `0.75` = "V 0.75", 
    `0.466` = "LAI 0.467", `1.488`= "LAI 1.349", `2.916`= "LAI 2.917", `4.402`= "LAI 4.485", `5.485`= "LAI 5.395")
)

mn_outputs_A %>% 
  select(F, absPAR, H, V, L) %>%
  ggplot(aes(F, absPAR)) + 
  geom_point(aes(colour = as.factor(H))) + 
  geom_line(aes(colour = as.factor(H))) + 
  facet_grid(rows = vars(V), cols = vars(L), labeller = names) +
  labs(title = "PAR", x = "FT height", y = "Absorbed PAR", colour = "Height")



