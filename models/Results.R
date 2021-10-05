library(Metrics)
names <- as_labeller(
  c(`0` = "0 CV", `0.1` = "0.1 CV", `0.25` = "0.25 CV", `0.5` = "0.5 CV", 
    `0.44` = "0.44 LAI", `1.521`= "1.429 LAI", `2.916`= "2.98 LAI", `4.556`= "4.238 LAI", `5.402`= "5.402 LAI")
)

maespa_n_others_fla_0.1$model <- factor(maespa_n_others_fla_0.1$model, levels = c("MAESPA", "PPA", "FLAT TOP", "DEEP CROWN"))
maespa_n_others_stand_fla_0.1$model <- factor(maespa_n_others_stand_fla_0.1$model, levels = c("MAESPA", "PPA", "FLAT TOP", "DEEP CROWN"))
maespa_n_others_c_fla$model <- factor(maespa_n_others_c_fla$model, levels = c("MAESPA", "PPA", "FLAT TOP", "DEEP CROWN"))

#fla 0.1 
maespa_n_others_fla_0.1 %>% 
  select(F, absPAR_two_s, absPAR_one_s, model, V, L) %>%
  ggplot(aes(F, absPAR_two_s)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_grid(rows = vars(V), cols = vars(L), labeller = names) +
  labs(x = "Ratio focal tree height : stand", y = "Absorbed PAR (MJ tree-1 d-1)", 
       colour = "Canopy Approximation") +   
  scale_colour_manual(labels = c("MAESPA", "PPA" , "Flat Top","Deep Crown"), 
                      values = c("grey45","red", "#00BA38", "goldenrod2")) +
  theme(
    axis.ticks = element_line(colour = "grey"),
    panel.grid.major = element_line(colour = "grey", size=0.1),
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill="white"), 
    panel.border = element_rect(colour = "grey45", fill = NA), 
    legend.key = element_rect(fill = "white"),
    strip.text = element_text(family = "Helvetica", colour = "black", size = 9), 
    axis.title = element_text(family = "Helvetica", colour = "black",
                              size = 11), 
    legend.title= element_text(family = "Helvetica", colour = "black",
                               size = 11),
    legend.text = element_text(family = "Helvetica", colour = "black"),
    legend.title.align	= 0.5) 

maespa_n_others_fla_0.1 %>% 
  select(F, absPAR_two_s, absPAR_one_s, model, V, L) %>%
  ggplot(aes(F, absPAR_two_s)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_grid(rows = vars(V), cols = vars(L), labeller = names) +
  labs(x = "Ratio focal tree height : stand", y = "Absorbed PAR (MJ tree-1 d-1)", 
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


#changing focal tree la 
names_c_fla <- as_labeller(
  c(`0.1` = "0.1", `0.5` = "0.5", `1` = "1", 
    `10` = "10", `20` = "20", `40` = "40")
)

maespa_n_others_c_fla %>% 
  select(F, absPAR_two_s, model, fla, L) %>%
  ggplot(aes(F, absPAR_two_s)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_wrap(~fla, scales = "free", labeller = names_c_fla) +
  labs(x = "Ratio focal tree height : stand", y = "Absorbed PAR (MJ m-2 d-1)", colour = "Canopy Approximation") +  
  scale_colour_manual(labels = c("MAESPA", "PPA" , "Flat Top","Deep Crown"), 
                      values = c("grey45","red", "#00BA38", "goldenrod2")) +
  theme(
    axis.ticks = element_line(colour = "grey"),
    panel.grid.major = element_line(colour = "grey", size=0.1),
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill="white"), 
    panel.border = element_rect(colour = "grey45", fill = NA), 
    legend.key = element_rect(fill = "white"),
    strip.text = element_text(family = "Helvetica", colour = "black", size = 9), 
    axis.title = element_text(family = "Helvetica", colour = "black",
                              size = 11), 
    legend.title= element_text(family = "Helvetica", colour = "black",
                               size = 11),
    legend.text = element_text(family = "Helvetica", colour = "black"),
    legend.title.align	= 0.5) 

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


#stand 0.1
names_stand <- as_labeller(
  c(`0.1` = "0.1", `0.5` = "0.5", `1` = "1", 
    `10` = "10", `20` = "20", `40` = "40")
)

maespa_n_others_stand_fla_0.1 %>% 
  select(absPAR_two_s, model, V, L) %>%
  ggplot(aes(L, absPAR_two_s)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_wrap(~V) +
  labs(x = "LAI", y = "Absorbed PAR (MJ m-2 d-1)", 
       colour = "Canopy Approximation") +
  scale_colour_manual(labels = c("MAESPA", "PPA" , "Flat Top","Deep Crown"), 
                      values = c("grey45","red", "#00BA38", "goldenrod2")) +
  theme(
    axis.ticks = element_line(colour = "grey"),
    panel.grid.major = element_line(colour = "grey", size=0.1),
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill="white"), 
    panel.border = element_rect(colour = "grey45", fill = NA), 
    legend.key = element_rect(fill = "white"),
    strip.text = element_text(family = "Helvetica", colour = "black", size = 9), 
    axis.title = element_text(family = "Helvetica", colour = "black",
                              size = 11), 
    legend.title= element_text(family = "Helvetica", colour = "black",
                               size = 11),
    legend.text = element_text(family = "Helvetica", colour = "black"),
    legend.title.align	= 0.5) 

actual_stand_fla_0.1 <- maespa_n_others_stand_fla_0.1 %>% filter(model == "MAESPA")
predicted_ppa_stand_fla_0.1 <- maespa_n_others_stand_fla_0.1 %>% filter(model == "PPA")
predicted_ft_stand_fla_0.1 <- maespa_n_others_stand_fla_0.1 %>% filter(model == "FLAT TOP")
predicted_DC_stand_fla_0.1 <- maespa_n_others_stand_fla_0.1 %>% filter(model == "DEEP CROWN")

rmse(actual_stand_fla_0.1$absPAR_two_s, predicted_ppa_stand_fla_0.1$absPAR_two_s)
rmse(actual_stand_fla_0.1$absPAR_two_s, predicted_ft_stand_fla_0.1$absPAR_two_s)
rmse(actual_stand_fla_0.1$absPAR_two_s, predicted_DC_stand_fla_0.1$absPAR_two_s)

rss_ppa <- sum((predicted_ppa_stand_fla_0.1$absPAR_two_s - actual_stand_fla_0.1$absPAR_two_s)^2)
rss_ft <- sum((predicted_ft_stand_fla_0.1$absPAR_two_s - actual_stand_fla_0.1$absPAR_two_s)^2)
rss_DC <- sum((predicted_DC_stand_fla_0.1$absPAR_two_s - actual_stand_fla_0.1$absPAR_two_s)^2)
tss <- sum((actual_stand_fla_0.1$absPAR_two_s- mean(actual_stand_fla_0.1$absPAR_two_s))^2)
rsq_ppa <- 1 - rss_ppa/tss
rsq_ft <- 1 - rss_ft/tss
rsq_DC <- 1 - rss_DC/tss


