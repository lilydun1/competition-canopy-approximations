library(Metrics)
names <- as_labeller(
  c(`0` = "CV = 0", `0.1` = "CV = 0.1", `0.25` = "CV = 0.25", `0.5` = "CV = 0.5", 
    `0.44` = "LAI = 0.44 ", `1.521`= "LAI = 1.43", `2.916`= "LAI = 2.98 ", 
    `4.556`= "LAI = 4.24", `5.402`= "LAI = 5.40")
)

final_results_fla_0.1$model <- factor(final_results_fla_0.1$model, levels = c("MAESPA", "PPA", "FLAT TOP", "DEEP CROWN"))
final_results_stand_fla_0.1$model <- factor(final_results_stand_fla_0.1$model, levels = c("MAESPA", "PPA", "FLAT TOP", "DEEP CROWN"))
final_results_c_fla$model <- factor(final_results_c_fla$model, levels = c("MAESPA", "PPA", "FLAT TOP", "DEEP CROWN"))

#fla 0.1 
final_results_fla_0.1 %>% 
  select(F, absPAR_two_s, absPAR_one_s, model, V, L) %>%
  ggplot(aes(F, absPAR_two_s)) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_grid(rows = vars(V), cols = vars(L), labeller = names) +
  labs(x = "\nRatio focal tree height : stand", y = "Absorbed PAR (MJ m-2 d-1)\n", 
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
    strip.text = element_text(family = "Helvetica", colour = "black", size = 12), 
    axis.title = element_text(family = "Helvetica", colour = "black",
                              size = 15), 
    axis.text = element_text(family = "Helvetica", size = 11),
    legend.title= element_text(family = "Helvetica", colour = "black",
                               size = 15),
    legend.text = element_text(family = "Helvetica", colour = "black",
                               size = 12),
    legend.title.align	= 0.5) +
  scale_x_continuous(limits = c(0, 2)) +
  scale_y_continuous(limits = c(0, 6))

actual_fla_0.1 <- maespa_n_others_fla_0.1 %>% filter(model == "MAESPA")
predicted_ppa_fla_0.1 <- maespa_n_others_fla_0.1 %>% filter(model == "PPA")
predicted_ft_fla_0.1 <- maespa_n_others_fla_0.1 %>% filter(model == "FLAT TOP")
predicted_DC_fla_0.1 <- maespa_n_others_fla_0.1 %>% filter(model == "DEEP CROWN")

rmse(actual_fla_0.1$absPAR_two_s, predicted_ppa_fla_0.1$absPAR_two_s)
rmse(actual_fla_0.1$absPAR_two_s, predicted_ft_fla_0.1$absPAR_two_s)
rmse(actual_fla_0.1$absPAR_two_s, predicted_DC_fla_0.1$absPAR_two_s)

cor(actual_fla_0.1$absPAR_two_s, predicted_ppa_fla_0.1$absPAR_two_s)^2
cor(actual_fla_0.1$absPAR_two_s, predicted_ft_fla_0.1$absPAR_two_s)^2
cor(actual_fla_0.1$absPAR_two_s, predicted_DC_fla_0.1$absPAR_two_s)^2

#changing focal tree la 
names_c_fla <- as_labeller(
  c(`0.1` = "Focal LA = 0.1", `0.5` = "Focal LA = 0.5", `1` = "Focal LA = 1", 
    `10` = "Focal LA = 10", `20` = "Focal LA = 20", `40` = "Focal LA = 40")
)

final_results_c_fla %>% 
  select(F, absPAR_two_s, model, fla, L) %>%
  ggplot(aes(F, absPAR_two_s)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_wrap(~fla, nrow = 2, labeller = names_c_fla) +
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

actual_c_fla <- maespa_n_others_c_fla %>% filter(model == "MAESPA")
predicted_ppa_c_fla <- maespa_n_others_c_fla %>% filter(model == "PPA")
predicted_ft_c_fla <- maespa_n_others_c_fla %>% filter(model == "FLAT TOP")
predicted_DC_c_fla <- maespa_n_others_c_fla %>% filter(model == "DEEP CROWN")

rmse(actual_c_fla$absPAR_two_s, predicted_ppa_c_fla$absPAR_two_s)
rmse(actual_c_fla$absPAR_two_s, predicted_ft_c_fla$absPAR_two_s)
rmse(actual_c_fla$absPAR_two_s, predicted_DC_c_fla$absPAR_two_s)

cor(actual_c_fla$absPAR_two_s, predicted_ppa_c_fla$absPAR_two_s)^2
cor(actual_c_fla$absPAR_two_s, predicted_ft_c_fla$absPAR_two_s)^2
cor(actual_c_fla$absPAR_two_s, predicted_DC_c_fla$absPAR_two_s)^2

#stand 0.1
names_stand <- as_labeller(
  c(`0` = "CV = 0", `0.1` = "CV = 0.1", `0.25` = "CV = 0.25", 
    `0.5` = "CV = 0.5"))

final_results_stand_fla_0.1 %>% 
  select(absPAR_two_s, model, V, L) %>%
  ggplot(aes(L, absPAR_two_s)) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_wrap(~V, labeller = names_stand) +
  labs(x = "\nLAI", y = "Absorbed PAR (MJ m-2 d-1)\n", 
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
    strip.text = element_text(family = "Helvetica", colour = "black", size = 10), 
    axis.title = element_text(family = "Helvetica", colour = "black",
                              size = 13), 
    axis.text = element_text(family = "Helvetica", size = 10),
    legend.title= element_text(family = "Helvetica", colour = "black",
                               size = 13),
    legend.text = element_text(family = "Helvetica", colour = "black", size = 10),
    legend.title.align	= 0.5) 

actual_stand_fla_0.1 <- final_results_stand_fla_0.1 %>% filter(model == "MAESPA")
predicted_ppa_stand_fla_0.1 <- final_results_stand_fla_0.1 %>% filter(model == "PPA")
predicted_ft_stand_fla_0.1 <- final_results_stand_fla_0.1 %>% filter(model == "FLAT TOP")
predicted_DC_stand_fla_0.1 <- final_results_stand_fla_0.1 %>% filter(model == "DEEP CROWN")

rmse(actual_stand_fla_0.1$absPAR_two_s, predicted_ppa_stand_fla_0.1$absPAR_two_s)
rmse(actual_stand_fla_0.1$absPAR_two_s, predicted_ft_stand_fla_0.1$absPAR_two_s)
rmse(actual_stand_fla_0.1$absPAR_two_s, predicted_DC_stand_fla_0.1$absPAR_two_s)

cor(actual_stand_fla_0.1$absPAR_two_s, predicted_ppa_stand_fla_0.1$absPAR_two_s)^2
cor(actual_stand_fla_0.1$absPAR_two_s, predicted_ft_stand_fla_0.1$absPAR_two_s)^2
cor(actual_stand_fla_0.1$absPAR_two_s, predicted_DC_stand_fla_0.1$absPAR_two_s)^2

#fla 27
maespa_n_others_fla_27 %>% 
  select(F, absPAR_two_s, absPAR_one_s, model, V, L) %>%
  ggplot(aes(F, absPAR_two_s)) + 
  geom_point(aes(colour = as.factor(model))) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_grid(rows = vars(V), cols = vars(L), labeller = names) +
  labs(title = "two stream - fla 27", x = "Ratio focal tree height : stand", y = "Absorbed PAR (MJ tree-1 d-1)", 
       colour = "Canopy Approximation") 
