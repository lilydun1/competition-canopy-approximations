names <- as_labeller(
  c(`0` = "V 0", `0.1` = "V 0.1", `0.25` = "V 0.25", `0.5` = "V 0.5", `0.75` = "V 0.75", 
    `0.466` = "LAI 0.467", `1.488`= "LAI 1.349", `2.916`= "LAI 2.917", `4.402`= "LAI 4.485", `5.485`= "LAI 5.395")
)

final_results_ft %>% 
  select(F, absPAR, H, V, L) %>%
  ggplot(aes(F, absPAR)) + 
  geom_point(aes(colour = as.factor(H))) + 
  geom_line(aes(colour = as.factor(H))) + 
  facet_grid(rows = vars(V), cols = vars(L), labeller = names) +
  labs(title = "PAR", x = "FT height", y = "Absorbed PAR", colour = "Flat top")

final_results_ppa %>% 
  select(F, absPAR, H, V, L) %>%
  ggplot(aes(F, absPAR)) + 
  geom_point(aes(colour = as.factor(H))) + 
  geom_line(aes(colour = as.factor(H))) + 
  facet_grid(rows = vars(V), cols = vars(L), labeller = names) +
  labs(title = "PAR", x = "FT height", y = "Absorbed PAR", colour = "Flat top")

final_results %>% 
  select(F, absPAR, model, V, L) %>%
  ggplot(aes(F, absPAR)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_grid(rows = vars(V), cols = vars(L), labeller = names) +
  labs(x = "Focal tree height", y = "Absorbed PAR", colour = "Model")

maespa_n_others_fla_0.1 %>% 
  select(F, absPAR, model, V, L) %>%
  ggplot(aes(F, absPAR)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_grid(rows = vars(V), cols = vars(L), labeller = names) +
  labs(x = "Focal tree height", y = "Absorbed PAR", colour = "Model")

maespa_n_others %>% 
  select(F, absPAR, model, V, L) %>%
  ggplot(aes(F, absPAR)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_grid(rows = vars(V), cols = vars(L), labeller = names) +
  labs(x = "Focal tree height", y = "Absorbed PAR", colour = "Model")

names_fla <- as_labeller(
  c(`0.1` = "f_la 0.1", `0.5` = "f_la 0.5", `1` = "f_la 1", 
    `10` = "f_la 10", `50` = "f_la 50")
)

final_results_new %>% 
  select(F, absPAR, model, fla, L) %>%
  ggplot(aes(F, absPAR)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_wrap(~fla, scales = "free", labeller = names_fla) +
  labs(x = "Focal tree height", y = "Absorbed PAR", colour = "Model")

maespa_n_others_fla %>% 
  select(F, absPAR, model, fla, L) %>%
  ggplot(aes(F, absPAR)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_wrap(~fla, scales = "free", labeller = names_fla) +
  labs(x = "Focal tree height", y = "Absorbed PAR", colour = "Model")

#filtered!
maespa_n_others_fla %>% 
  select(F, absPAR, model, fla, L) %>%
  filter(F == 1) %>% 
  ggplot(aes(F, absPAR)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_wrap(~fla, scales = "free", labeller = names_fla) +
  labs(x = "Focal tree height", y = "Absorbed PAR", colour = "Model")
