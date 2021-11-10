library(Metrics)
names <- as_labeller(
  c(`0` = "CV = 0", `0.1` = "CV = 0.1", `0.25` = "CV = 0.25", `0.5` = "CV = 0.5", 
    `0.44` = "LAI = 0.44 ", `1.521`= "LAI = 1.43", `2.916`= "LAI = 2.98 ", 
    `4.556`= "LAI = 4.24", `5.402`= "LAI = 5.40")
)

final_results_fla_0.1$model <- factor(final_results_fla_0.1$model, levels = c("MAESPA", "PPA", "FLAT TOP", "DEEP CROWN"))
final_results_stand_fla_0.1$model <- factor(final_results_stand_fla_0.1$model, levels = c("MAESPA", "PPA", "FLAT TOP", "DEEP CROWN"))
final_results_stand_fla_27$model <- factor(final_results_stand_fla_27$model, levels = c("MAESPA", "PPA", "FLAT TOP", "DEEP CROWN"))
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
    axis.title = element_text(family = "Helvetica", colour = "black",
                              size = 15), 
    axis.text = element_text(family = "Helvetica", size = 11),
    legend.key = element_rect(fill = "white"),
    legend.title= element_text(family = "Helvetica", colour = "black",
                               size = 15),
    legend.text = element_text(family = "Helvetica", colour = "black",
                               size = 13),
    legend.title.align	= 0.5,
    panel.border = element_rect(colour = "grey45", fill = NA), 
    panel.grid.major = element_line(colour = "grey", size=0.1),
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill="white"), 
    strip.text = element_text(family = "Helvetica", colour = "black", size = 13)) +
  scale_x_continuous(breaks = c(0, 0.5, 1, 1.5, 2), label = c(0, 0.5, 1, 1.5, 2)) +
  scale_y_continuous(limits = c(0.9, 5.6))

actual_fla_0.1 <- final_results_fla_0.1 %>% filter(model == "MAESPA")
predicted_ppa_fla_0.1 <- final_results_fla_0.1 %>% filter(model == "PPA")
predicted_ft_fla_0.1 <- final_results_fla_0.1 %>% filter(model == "FLAT TOP")
predicted_DC_fla_0.1 <- final_results_fla_0.1 %>% filter(model == "DEEP CROWN")

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
  geom_line(aes(colour = as.factor(model))) + 
  facet_wrap(~fla, nrow = 2, labeller = names_c_fla) +
  labs(x = "\nRatio focal tree height : stand", y = "Absorbed PAR (MJ m-2 d-1)\n", colour = "Canopy Approximation") +  
  scale_colour_manual(labels = c("MAESPA", "PPA" , "Flat Top","Deep Crown"), 
                      values = c("grey45","red", "#00BA38", "goldenrod2")) +
  theme(
    axis.ticks = element_line(colour = "grey"),
    axis.title = element_text(family = "Helvetica", colour = "black",
                              size = 15), 
    axis.text = element_text(family = "Helvetica", size = 11),
    legend.key = element_rect(fill = "white"),
    legend.title= element_text(family = "Helvetica", colour = "black",
                               size = 15),
    legend.text = element_text(family = "Helvetica", colour = "black",
                               size = 13),
    legend.title.align	= 0.5,
    panel.border = element_rect(colour = "grey45", fill = NA), 
    panel.grid.major = element_line(colour = "grey", size=0.1),
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill="white"), 
    strip.text = element_text(family = "Helvetica", colour = "black", size = 13))+
  scale_x_continuous(breaks = c(0, 0.5, 1, 1.5, 2), label = c(0, 0.5, 1, 1.5, 2)) +
  scale_y_continuous(limits = c(1.5, 5.6))


thing <- c(0.1, 40)

final_results_c_fla %>% 
  select(F, absPAR_two_s, model, fla, L) %>%
  filter(fla %in% thing) %>% 
  ggplot(aes(F, absPAR_two_s)) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_wrap(~fla, nrow = 1, labeller = names_c_fla) +
  labs(x = "\nRatio focal tree height : stand", y = "Absorbed PAR (MJ m-2 d-1)\n", colour = "Canopy Approximation") +  
  scale_colour_manual(labels = c("MAESPA", "PPA" , "Flat Top","Deep Crown"), 
                      values = c("grey45","red", "#00BA38", "goldenrod2")) +
  theme(
    axis.ticks = element_line(colour = "grey"),
    axis.title = element_text(family = "Helvetica", colour = "black",
                              size = 15), 
    axis.text = element_text(family = "Helvetica", size = 11),
    legend.key = element_rect(fill = "white"),
    legend.title= element_text(family = "Helvetica", colour = "black",
                               size = 15),
    legend.text = element_text(family = "Helvetica", colour = "black",
                               size = 13),
    legend.title.align	= 0.5,
    panel.border = element_rect(colour = "grey45", fill = NA), 
    panel.grid.major = element_line(colour = "grey", size=0.1),
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill="white"), 
    strip.text = element_text(family = "Helvetica", colour = "black", size = 13)) +
   scale_x_continuous(breaks = c(0, 0.5, 1, 1.5, 2), label = c(0, 0.5, 1, 1.5, 2)) 

mode_thing <- c("PPA", "FLAT TOP", "DEEP CROWN")

df <- final_results_c_fla %>% 
  select(F, model, fla, L, absPAR_two_s) %>%
  filter(F == 1.99 & model %in% mode_thing) %>% 
  group_by(F, fla, L) %>% 
  summarise(absPAR_two_s = mean(absPAR_two_s))

df_PPA <- final_results_c_fla %>% 
  select(F, model, fla, L, absPAR_two_s) %>%
  filter(F == 1.99 & model == "DEEP CROWN") 

df_maespa <- final_results_c_fla %>% 
  select(F, model, fla, L, absPAR_two_s) %>%
  filter(F == 1.99 & model == "MAESPA") 

df_ppo <- df_PPA %>% 
  add_column(diff = df_PPA$absPAR_two_s - df_maespa$absPAR_two_s)

df_ppo %>% 
  ggplot(aes(fla, diff)) + 
  geom_line(aes(colour = as.factor(L)))  +
  labs(x = "\n Focal tree LA (m2)", y = "Mean difference of absorbed PAR (MJ m-2 d-1)\n", colour = "Canopy Approximation") +
  scale_colour_manual(values = c("darkgreen")) +
  theme(
    axis.ticks = element_line(colour = "grey"),
    axis.text = element_text(family = "Helvetica", size = 11),
    axis.title = element_text(family = "Helvetica", colour = "black",
                              size = 15),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_line(colour = "grey", size=0.1),
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "white", fill = NA), 
    legend.key = element_rect(fill = "white"),
    legend.title= element_text(family = "Helvetica", colour = "black",
                               size = 15),
    legend.text = element_text(family = "Helvetica", colour = "black",
                               size = 12),
    legend.title.align	= 0.5)  +
  scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2), label = c(0, 0.5, 1, 1.5, 2)) 


actual_0.1_c_fla <- final_results_c_fla %>% 
  filter(model == "MAESPA", fla == 0.1)
predicted_DC_0.1_c_fla <- final_results_c_fla %>% 
  filter(model == "DEEP CROWN", fla == 0.1)
predicted_FT_0.1_c_fla <- final_results_c_fla %>% 
  filter(model == "FLAT TOP", fla == 0.1)
predicted_PPA_0.1_c_fla <- final_results_c_fla %>% 
  filter(model == "PPA", fla == 0.1)


actual_40_c_fla <- final_results_c_fla %>% 
  filter(model == "MAESPA", fla == 40)
predicted_DC_40_c_fla <- final_results_c_fla %>% 
  filter(model == "DEEP CROWN", fla == 40)
predicted_FT_40_c_fla <- final_results_c_fla %>% 
  filter(model == "FLAT TOP", fla == 40)
predicted_PPA_40_c_fla <- final_results_c_fla %>% 
  filter(model == "PPA", fla == 40)

rmse(actual_0.1_c_fla$absPAR_two_s, predicted_PPA_0.1_c_fla$absPAR_two_s)
rmse(actual_0.1_c_fla$absPAR_two_s, predicted_FT_0.1_c_fla$absPAR_two_s)
rmse(actual_0.1_c_fla$absPAR_two_s, predicted_DC_0.1_c_fla$absPAR_two_s)

cor(actual_0.1_c_fla$absPAR_two_s, predicted_PPA_0.1_c_fla$absPAR_two_s)^2
cor(actual_0.1_c_fla$absPAR_two_s, predicted_FT_0.1_c_fla$absPAR_two_s)^2
cor(actual_0.1_c_fla$absPAR_two_s, predicted_DC_0.1_c_fla$absPAR_two_s)^2

rmse(actual_40_c_fla$absPAR_two_s, predicted_PPA_40_c_fla$absPAR_two_s)
rmse(actual_40_c_fla$absPAR_two_s, predicted_FT_40_c_fla$absPAR_two_s)
rmse(actual_40_c_fla$absPAR_two_s, predicted_DC_40_c_fla$absPAR_two_s)

cor(actual_40_c_fla$absPAR_two_s, predicted_PPA_40_c_fla$absPAR_two_s)^2
cor(actual_40_c_fla$absPAR_two_s, predicted_FT_40_c_fla$absPAR_two_s)^2
cor(actual_40_c_fla$absPAR_two_s, predicted_DC_40_c_fla$absPAR_two_s)^2

#stand 27
names_stand <- as_labeller(
  c(`0` = "CV = 0", `0.1` = "CV = 0.1", `0.25` = "CV = 0.25", 
    `0.5` = "CV = 0.5"))


final_results_stand_fla_27 %>% 
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
    axis.title = element_text(family = "Helvetica", colour = "black",
                              size = 15), 
    axis.text = element_text(family = "Helvetica", size = 11),
    legend.key = element_rect(fill = "white"),
    legend.title= element_text(family = "Helvetica", colour = "black",
                               size = 15),
    legend.text = element_text(family = "Helvetica", colour = "black",
                               size = 13),
    legend.title.align	= 0.5,
    panel.border = element_rect(colour = "grey45", fill = NA), 
    panel.grid.major = element_line(colour = "grey", size=0.1),
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill="white"), 
    strip.text = element_text(family = "Helvetica", colour = "black", size = 13))

actual_stand_fla_27 <- final_results_stand_fla_27 %>% filter(model == "MAESPA")
predicted_ppa_stand_fla_27 <- final_results_stand_fla_27 %>% filter(model == "PPA")
predicted_ft_stand_fla_27 <- final_results_stand_fla_27 %>% filter(model == "FLAT TOP")
predicted_DC_stand_fla_27 <- final_results_stand_fla_27 %>% filter(model == "DEEP CROWN")

rmse(actual_stand_fla_27$absPAR_two_s, predicted_ppa_stand_fla_27$absPAR_two_s)
rmse(actual_stand_fla_27$absPAR_two_s, predicted_ft_stand_fla_27$absPAR_two_s)
rmse(actual_stand_fla_27$absPAR_two_s, predicted_DC_stand_fla_27$absPAR_two_s)

cor(actual_stand_fla_27$absPAR_two_s, predicted_ppa_stand_fla_27$absPAR_two_s)^2
cor(actual_stand_fla_27$absPAR_two_s, predicted_ft_stand_fla_27$absPAR_two_s)^2
cor(actual_stand_fla_27$absPAR_two_s, predicted_DC_stand_fla_27$absPAR_two_s)^2


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
    axis.title = element_text(family = "Helvetica", colour = "black",
                              size = 15), 
    axis.text = element_text(family = "Helvetica", size = 11),
    legend.key = element_rect(fill = "white"),
    legend.title= element_text(family = "Helvetica", colour = "black",
                               size = 15),
    legend.text = element_text(family = "Helvetica", colour = "black",
                               size = 13),
    legend.title.align	= 0.5,
    panel.border = element_rect(colour = "grey45", fill = NA), 
    panel.grid.major = element_line(colour = "grey", size=0.1),
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill="white"), 
    strip.text = element_text(family = "Helvetica", colour = "black", size = 13))

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
