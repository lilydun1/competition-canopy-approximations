names <- as_labeller(
  c(`0` = "V 0", `0.1` = "V 0.1", `0.25` = "V 0.25", `0.5` = "V 0.5", `0.75` = "V 0.75", 
    `0.466` = "LAI 0.467", `1.488`= "LAI 1.349", `2.916`= "LAI 2.917", `4.402`= "LAI 4.485", `5.485`= "LAI 5.395")
)

maespa_n_others_fla_0.1 %>% 
  select(F, absPAR_two_s, absPAR_one_s, model, V, L) %>%
  ggplot(aes(F, absPAR_two_s)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_grid(rows = vars(V), cols = vars(L), labeller = names) +
  labs(title = "two stream", x = "Focal tree height", y = "Absorbed PAR (MJ tree-1 d-1)", colour = "Model") 

maespa_n_others_fla_0.1 %>% 
  select(F, absPAR_two_s, absPAR_one_s, model, V, L) %>%
  filter(V == 0) %>% 
  ggplot(aes(F, absPAR_two_s)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_grid(rows = vars(V), cols = vars(L), labeller = names) +
  labs(x = "Focal tree height", y = "Absorbed PAR (MJ tree-1 d-1)", colour = "Model") 

maespa_n_others_fla_0.1 %>% 
  select(F, absPAR_two_s, absPAR_one_s, model, V, L) %>%
  filter(V == 0, L == 0.466) %>% 
  ggplot(aes(F, absPAR_two_s)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  labs(x = "Focal tree height", y = "Absorbed PAR (MJ tree-1 d-1)", colour = "Model") 

maespa_n_others_fla_0.1 %>% 
  select(F, absPAR_two_s, absPAR_one_s, model, V, L) %>%
  filter(V == 0, L == 1.488) %>% 
  ggplot(aes(F, absPAR_two_s)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  labs(x = "Focal tree height", y = "Absorbed PAR (MJ tree-1 d-1)", colour = "Model") 

maespa_n_others_fla_0.1 %>% 
  select(F, absPAR_two_s, absPAR_one_s, model, V, L) %>%
  filter(V == 0, L == 2.916) %>% 
  ggplot(aes(F, absPAR_two_s)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  labs(x = "Focal tree height", y = "Absorbed PAR (MJ tree-1 d-1)", colour = "Model") 

maespa_n_others_fla_0.1 %>% 
  select(F, absPAR_two_s, absPAR_one_s, model, V, L) %>%
  filter(V == 0, L == 4.402) %>% 
  ggplot(aes(F, absPAR_two_s)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  labs(x = "Focal tree height", y = "Absorbed PAR (MJ tree-1 d-1)", colour = "Model") 


maespa_n_others_fla_0.1 %>% 
  select(F, absPAR_two_s, absPAR_one_s, model, V, L) %>%
  filter(V == 0, L == 5.485) %>% 
  ggplot(aes(F, absPAR_two_s)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  labs(x = "Focal tree height", y = "Absorbed PAR (MJ tree-1 d-1)", colour = "Model") 

maespa_n_others_fla_0.1 %>% 
  select(F, absPAR_one_s, model, V, L) %>%
  ggplot(aes(F, absPAR_one_s)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_grid(rows = vars(V), cols = vars(L), labeller = names) +
  labs(title = "one stream", x = "Focal tree height", y = "Absorbed PAR", colour = "Model")

maespa_n_others_fla_47.62 %>% 
  select(F, absPAR_two_s, model, V, L) %>%
  ggplot(aes(F, absPAR_two_s)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_grid(rows = vars(V), cols = vars(L), labeller = names) +
  labs(title = "no self-shading", x = "Focal tree height", y = "Absorbed PAR", colour = "Model")

names_fla <- as_labeller(
  c(`0.1` = "f_la 0.1", `0.5` = "f_la 0.5", `1` = "f_la 1", 
    `10` = "f_la 10", `50` = "f_la 50")
)

maespa_n_others_c_fla %>% 
  select(F, absPAR_two_s, model, fla, L) %>%
  ggplot(aes(F, absPAR_two_s)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_wrap(~fla, scales = "free", labeller = names_fla) +
  labs(x = "Focal tree height", y = "Absorbed PAR", colour = "Model")

