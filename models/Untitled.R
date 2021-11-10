final_results_fla_0.1$model <- factor(final_results_fla_0.1$model, levels = c("MAESPA", "PPA", "FLAT TOP", "DEEP CROWN"))

final_results_fla_0.1 %>% 
  select(F, absPAR_two_s, absPAR_one_s, model, V, L) %>%
  ggplot(aes(F, absPAR_two_s)) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_grid(rows = vars(V), cols = vars(L), labeller = names) +
  ylab(bquote(atop(Absorbed~PAR~(MJ~ m^-2~d^-1), "."))) +
  labs(x = "\nRatio focal tree height : stand", colour = "Canopy Approximation") +   
  scale_colour_manual(labels = c("MAESPA", "PPA" , "Flat Top","Deep Crown"), 
                      values = c("grey45","red", "#00BA38", "goldenrod2")) +
  theme(
    axis.ticks = element_line(colour = "grey"),
    axis.title = element_text(family = "Helvetica", colour = "black",
                              size = 18), 
    axis.text = element_text(family = "Helvetica", size = 14),
    legend.key = element_rect(fill = "white"),
    legend.title= element_text(family = "Helvetica", colour = "black",
                               size = 18),
    legend.text = element_text(family = "Helvetica", colour = "black",
                               size = 14),
    legend.title.align	= 0.5,
    panel.border = element_rect(colour = "grey45", fill = NA), 
    panel.grid.major = element_line(colour = "grey", size=0.1),
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill="white"), 
    strip.text = element_text(family = "Helvetica", colour = "black", size = 15)) +
  scale_x_continuous(breaks = c(0, 0.5, 1, 1.5, 2), label = c(0, 0.5, 1, 1.5, 2)) +
  scale_y_continuous(limits = c(0.9, 5.6))

final_results_c_fla$model <- factor(final_results_c_fla$model, levels = c("MAESPA", "PPA", "FLAT TOP", "DEEP CROWN"))

thing <- c(0.1, 40)

final_results_c_fla %>% 
  select(F, absPAR_two_s, model, fla, L) %>%
  filter(fla %in% thing) %>% 
  ggplot(aes(F, absPAR_two_s)) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_wrap(~fla, nrow = 1, labeller = names_c_fla) +
  ylab(bquote(atop(Absorbed~PAR~(MJ~ m^-2~d^-1), "."))) +
  labs(x = "\nRatio focal tree height : stand", colour = "Canopy Approximation") +  
  scale_colour_manual(labels = c("MAESPA", "PPA" , "Flat Top","Deep Crown"), 
                      values = c("grey45","red", "#00BA38", "goldenrod2")) +
  theme(
    axis.ticks = element_line(colour = "grey"),
    axis.title = element_text(family = "Helvetica", colour = "black",
                              size = 18), 
    axis.text = element_text(family = "Helvetica", size = 14),
    legend.key = element_rect(fill = "white"),
    legend.title= element_text(family = "Helvetica", colour = "black",
                               size = 18),
    legend.text = element_text(family = "Helvetica", colour = "black",
                               size = 14),
    legend.title.align	= 0.5,
    panel.border = element_rect(colour = "grey45", fill = NA), 
    panel.grid.major = element_line(colour = "grey", size=0.1),
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill="white"), 
    strip.text = element_text(family = "Helvetica", colour = "black", size = 15)) +
  scale_x_continuous(breaks = c(0, 0.5, 1, 1.5, 2), label = c(0, 0.5, 1, 1.5, 2)) 

df_ppo %>% 
  ggplot(aes(fla, diff)) + 
  geom_line(aes(colour = as.factor(L))) +
  ylab(bquote(atop(Absorbed~PAR~(MJ~ m^-2~d^-1), "."))) +
  xlab(bquote(Focal~tree~LA~(m^2))) +
  labs(colour = "Canopy Approximation") +
  scale_colour_manual(values = c("darkgreen")) +
  theme(
    axis.ticks = element_line(colour = "grey"),
    axis.text = element_text(family = "Helvetica", size = 14),
    axis.title = element_text(family = "Helvetica", colour = "black",
                              size = 18),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_line(colour = "grey", size=0.1),
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "white", fill = NA), 
    legend.key = element_rect(fill = "white"),
    legend.title= element_text(family = "Helvetica", colour = "black",
                               size = 18),
    legend.text = element_text(family = "Helvetica", colour = "black",
                               size = 14),
    legend.title.align	= 0.5)  +
  scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2), label = c(0, 0.5, 1, 1.5, 2)) 

final_results_stand_fla_27$model <- factor(final_results_stand_fla_27$model, levels = c("MAESPA", "PPA", "FLAT TOP", "DEEP CROWN"))

final_results_stand_fla_27 %>% 
  select(absPAR_two_s, model, V, L) %>%
  ggplot(aes(L, absPAR_two_s)) + 
  geom_line(aes(colour = as.factor(model))) + 
  facet_wrap(~V, labeller = names_stand)  +
  ylab(bquote(atop(Absorbed~PAR~(MJ~ m^-2~d^-1), "."))) +
  labs(x = "\n LAI", colour = "Canopy Approximation") +
  scale_colour_manual(labels = c("MAESPA", "PPA" , "Flat Top","Deep Crown"), 
                      values = c("grey45","red", "#00BA38", "goldenrod2")) +
  theme(
    axis.ticks = element_line(colour = "grey"),
    axis.title = element_text(family = "Helvetica", colour = "black",
                              size = 18), 
    axis.text = element_text(family = "Helvetica", size = 14),
    legend.key = element_rect(fill = "white"),
    legend.title= element_text(family = "Helvetica", colour = "black",
                               size = 18),
    legend.text = element_text(family = "Helvetica", colour = "black",
                               size = 14),
    legend.title.align	= 0.5,
    panel.border = element_rect(colour = "grey45", fill = NA), 
    panel.grid.major = element_line(colour = "grey", size=0.1),
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill="white"), 
    strip.text = element_text(family = "Helvetica", colour = "black", size = 15))

df <- c("DEEP CROWN", "MAESPA")

final_results_fla_0.1 %>% 
  filter(model %in% df & L == 2.916 & V == 0.1) %>% 
  select(F, absPAR_one_s, absPAR_two_s, model) %>% 
  pivot_longer(cols = starts_with("abs"), names_to = "absPAR_one_s") %>% 
  rename(stream = absPAR_one_s, absPAR = value) %>% 
  ggplot(aes(F, absPAR)) + 
  geom_point(aes(colour = factor(model))) 
