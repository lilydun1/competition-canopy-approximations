names <- as_labeller(
  c(`0` = "V 0", `0.1` = "V 0.1", `0.25` = "V 0.25", `0.5` = "V 0.5", `0.75` = "V 0.75", 
    `0.466` = "LAI 0.467", `1.488`= "LAI 1.349", `2.916`= "LAI 2.917", `4.402`= "LAI 4.485", `5.485`= "LAI 5.395")
)


#average
mn_outputs_A %>% 
  select(F, absPAR, H, V, L) %>%
  ggplot(aes(F, absPAR)) + 
  geom_point(aes(colour = as.factor(H))) + 
  geom_line(aes(colour = as.factor(H))) + 
  facet_grid(rows = vars(V), cols = vars(L), labeller = names) +
  labs(x = "FT height", y = "Absorbed PAR", colour = "MAESPA") +
  scale_color_manual(values = c("#00AFBB"))


mn_outputs_WD %>% 
  select(F, totPs, H, V, L, WD) %>%
  ggplot(aes(F, totPs)) + 
  geom_point(aes(colour = as.factor(WD))) + 
  geom_line(aes(colour = as.factor(WD))) +
  facet_grid(rows = vars(V), cols = vars(L), labeller = names) +
  labs(title = "photosynthesis", x = "FT height", y = "Gross photosynthesis", colour = "Wet or Dry")
