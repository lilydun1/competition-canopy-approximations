names <- as_labeller(
  c(`0` = "V 0", `0.1` = "V 0.1", `0.25` = "V 0.25", `0.5` = "V 0.5", `0.75` = "V 0.75", 
    `0.466` = "LAI 0.467", `1.488`= "LAI 1.349", `2.916`= "LAI 2.917", `4.402`= "LAI 4.485", `5.485`= "LAI 5.395")
)


mn_outputs_fla_0.1 %>% 
  select(F, absPAR, H, V, L) %>%
  filter(V == 0 & L == 5.061) %>% 
  ggplot(aes(F, absPAR)) + 
  geom_point(aes(colour = as.factor(fla))) + 
  geom_line(aes(colour = as.factor(fla))) + 
  facet_grid(rows = vars(V), cols = vars(L), labeller = names) +
  labs(x = "FT height", y = "Absorbed PAR", colour = "MAESPA")

mn_outputs_c_fla %>% 
  select(F, absPAR, H, V, L, fla) %>%
  ggplot(aes(F, absPAR)) + 
  geom_point(aes(colour = as.factor(fla))) + 
  geom_line(aes(colour = as.factor(fla))) +
  facet_grid(rows = vars(V), cols = vars(L), labeller = names) +
  labs(title = "photosynthesis", x = "FT height", y = "Gross photosynthesis", colour = "Wet or Dry")

mn_outputs_fla_0.1 %>% 
  select(F, absPAR, H, V, L) %>% 
  ggplot(aes(F, absPAR)) + 
  geom_point(aes(colour = as.factor(fla))) + 
  geom_line(aes(colour = as.factor(fla))) + 
  facet_grid(rows = vars(V), cols = vars(L)) +
  labs(x = "FT height", y = "Absorbed PAR", colour = "MAESPA")

output_combined_fla_0.1 %>% 
  select(F, absPAR, H, V, L) %>% 
  ggplot(aes(F, absPAR)) + 
  geom_point(aes(colour = as.factor(fla))) + 
  geom_line(aes(colour = as.factor(fla))) + 
  facet_grid(rows = vars(V), cols = vars(L)) +
  labs(x = "FT height", y = "Absorbed PAR", colour = "MAESPA")
