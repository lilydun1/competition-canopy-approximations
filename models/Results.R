names <- as_labeller(
  c(`0` = "V 0", `0.1` = "V 0.1", `0.25` = "V 0.25", `0.5` = "V 0.5", `0.75` = "V 0.75", 
    `0.44` = "LAI 0.44", `1.521`= "LAI 1.429", `2.916`= "LAI 2.98", `4.556`= "LAI 4.238", `5.402`= "LAI 5.402")
)

#fla 0.1 
maespa_n_others_fla_0.1 %>% 
  select(F, absPAR_two_s, absPAR_one_s, model, V, L) %>%
  ggplot(aes(F, absPAR_two_s)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_grid(rows = vars(V), cols = vars(L), labeller = names) +
  labs(title = "two stream - fla 0.1", x = "Ratio focal tree height : stand", y = "Absorbed PAR (MJ tree-1 d-1)", 
       colour = "Canopy Approximation") 

maespa_n_others_fla_0.1 %>% 
  select(F, absPAR_two_s, absPAR_one_s, model, V, L) %>%
  ggplot(aes(F, absPAR_one_s)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_grid(rows = vars(V), cols = vars(L), labeller = names) +
  labs(title = "one stream - fla 0.1", x = "Ratio focal tree height : stand", y = "Absorbed PAR (MJ tree-1 d-1)", 
       colour = "Canopy Approximation") 

actual_fla_0.1 <- maespa_n_others_fla_0.1 %>% filter(model == "MAESPA")
predicted_ppa_fla_0.1 <- maespa_n_others_fla_0.1 %>% filter(model == "PPA")
predicted_ft_fla_0.1 <- maespa_n_others_fla_0.1 %>% filter(model == "FLAT TOP")
predicted_DC_fla_0.1 <- maespa_n_others_fla_0.1 %>% filter(model == "DEEP CROWN")

rmse(actual_fla_0.1$absPAR_two_s, predicted_ppa_fla_0.1$absPAR_two_s)
rmse(actual_fla_0.1$absPAR_two_s, predicted_ft_fla_0.1$absPAR_two_s)
rmse(actual_fla_0.1$absPAR_two_s, predicted_DC_fla_0.1$absPAR_two_s)

rmse(actual_fla_0.1$absPAR_one_s, predicted_ppa_fla_0.1$absPAR_one_s)
rmse(actual_fla_0.1$absPAR_one_s, predicted_ft_fla_0.1$absPAR_one_s)
rmse(actual_fla_0.1$absPAR_one_s, predicted_DC_fla_0.1$absPAR_one_s)

#fla 27
maespa_n_others_fla_27 %>% 
  select(F, absPAR_two_s, absPAR_one_s, model, V, L) %>%
  ggplot(aes(F, absPAR_two_s)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_grid(rows = vars(V), cols = vars(L), labeller = names) +
  labs(title = "two stream - fla 27", x = "Ratio focal tree height : stand", y = "Absorbed PAR (MJ tree-1 d-1)", 
       colour = "Canopy Approximation") 

maespa_n_others_fla_27 %>% 
  select(F, absPAR_two_s, absPAR_one_s, model, V, L) %>%
  ggplot(aes(F, absPAR_one_s)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_grid(rows = vars(V), cols = vars(L), labeller = names) +
  labs(title = "one stream - fla 27", x = "Ratio focal tree height : stand", y = "Absorbed PAR (MJ tree-1 d-1)", 
       colour = "Canopy Approximation") 

actual_fla_27 <- maespa_n_others_fla_27 %>% filter(model == "MAESPA")
predicted_ppa_fla_27 <- maespa_n_others_fla_27 %>% filter(model == "PPA")
predicted_ft_fla_27 <- maespa_n_others_fla_27 %>% filter(model == "FLAT TOP")
predicted_DC_fla_27 <- maespa_n_others_fla_27 %>% filter(model == "DEEP CROWN")

rmse(actual_fla_27$absPAR_two_s, predicted_ppa_fla_27$absPAR_two_s)
rmse(actual_fla_27$absPAR_two_s, predicted_ft_fla_27$absPAR_two_s)
rmse(actual_fla_27$absPAR_two_s, predicted_DC_fla_27$absPAR_two_s)

rmse(actual_fla_27$absPAR_one_s, predicted_ppa_fla_27$absPAR_one_s)
rmse(actual_fla_27$absPAR_one_s, predicted_ft_fla_27$absPAR_one_s)
rmse(actual_fla_27$absPAR_one_s, predicted_DC_fla_27$absPAR_one_s)

#changing focal tree la 
names_c_fla <- as_labeller(
  c(`0.1` = "f_la 0.1", `0.5` = "f_la 0.5", `1` = "f_la 1", 
    `10` = "f_la 10", `20` = "f_la 20", `40` = "f_la 40")
)

maespa_n_others_c_fla %>% 
  select(F, absPAR_two_s, model, fla, L) %>%
  ggplot(aes(F, absPAR_two_s)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_wrap(~fla, scales = "free", labeller = names_c_fla) +
  labs(x = "Focal tree height", y = "Absorbed PAR", colour = "Model")

actual_c_fla <- maespa_n_others_c_fla %>% filter(model == "MAESPA")
predicted_ppa_c_fla <- maespa_n_others_c_fla %>% filter(model == "PPA")
predicted_ft_c_fla <- maespa_n_others_c_fla %>% filter(model == "FLAT TOP")
predicted_DC_c_fla <- maespa_n_others_c_fla %>% filter(model == "DEEP CROWN")

rmse(actual_c_fla$absPAR_two_s, predicted_ppa_c_fla$absPAR_two_s)
rmse(actual_c_fla$absPAR_two_s, predicted_ft_c_fla$absPAR_two_s)
rmse(actual_c_fla$absPAR_two_s, predicted_DC_c_fla$absPAR_two_s)

rmse(actual_c_fla$absPAR_one_s, predicted_ppa_c_fla$absPAR_one_s)
rmse(actual_c_fla$absPAR_one_s, predicted_ft_c_fla$absPAR_one_s)
rmse(actual_c_fla$absPAR_one_s, predicted_DC_c_fla$absPAR_one_s)

#stand 0.1
maespa_n_others_stand_fla_0.1 %>% 
  select(F, absPAR_two_s, model, V, L) %>%
  ggplot(aes(F, absPAR_two_s)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_grid(rows = vars(V), cols = vars(L), labeller = names) +
  labs(title = "two stream - stand 0.1", x = "Focal tree height", y = "Absorbed PAR (MJ tree-1 d-1)", 
       colour = "Canopy Approximation")

actual_stand_fla_0.1 <- maespa_n_others_stand_fla_0.1 %>% filter(model == "MAESPA")
predicted_ppa_stand_fla_0.1 <- maespa_n_others_stand_fla_0.1 %>% filter(model == "PPA")
predicted_ft_stand_fla_0.1 <- maespa_n_others_stand_fla_0.1 %>% filter(model == "FLAT TOP")

rmse(actual_stand_fla_0.1$absPAR_two_s, predicted_ppa_stand_fla_0.1$absPAR_two_s)
rmse(actual_stand_fla_0.1$absPAR_two_s, predicted_ft_stand_fla_0.1$absPAR_two_s)

rmse(actual_stand_fla_0.1$absPAR_one_s, predicted_ppa_stand_fla_0.1$absPAR_one_s)
rmse(actual_stand_fla_0.1$absPAR_one_s, predicted_ft_stand_fla_0.1$absPAR_one_s)



