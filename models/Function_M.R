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
      light_ppa = exp(-0.5*lai_ppa)) %>% 
    as_tibble()
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

PAR_calculator_ft <- function(data, indi_la = 47.62) {
  met <- readmet(filename = "met.dat", nlines = -1)
  la = indi_la
  L_ft = data %>% 
    filter(focal == "TRUE") %>% 
    pull(lai) 
  PAR_calculator <- 
    tibble(
      PAR_inst = met$PAR, 
      time = met$TIME) %>% 
    mutate(
      delta_t = c(diff(time[1:2]), diff(time)),
      absPAR_inst = PAR_inst*exp(-0.5*L_ft),#mu PAR m-2 s-1
      W = absPAR_inst/2.02, #W PAR m-2 eqn 38 from paper 
      MJ = W*0.0036 #MJ PAR m-2 h-1 eqn 12 from paper
    ) %>% 
    summarise(absPAR = (sum(MJ*delta_t)*la)) #MJ PAR tree-1 day-1 
}

#trying to get same result as MAESPA with a single tree with a la of 0.1 
#try is the lone tree with 0.1 la simulation folder within the folder simualtions 
maespa_0.1_result = 0.5614
df <- "simulations/try" %>% 
  load_trees()
df <- model_simp_f(df)
d$focal = TRUE
df <- PAR_calculator_ft(d, indi_la = 0.1)

PAR_calculator_ppa <- function(data,indi_la = 47.62) {
  met <- readmet(filename = "met.dat", nlines = -1)
  la = indi_la
  L_ft = data %>% 
    filter(focal == "TRUE") %>% 
    pull(lai_ppa) 
  PAR_calculator <- 
    tibble(
      PAR_inst = met$PAR, 
      time = met$TIME) %>% 
    mutate(
      delta_t = c(diff(time[1:2]), diff(time)),
      absPAR_inst = PAR_inst*exp(-0.5*L_ft),#mu PAR m-2 s-1
      W = absPAR_inst/2.02, #W PAR m-2 eqn 38 from paper 
      MJ = W*0.0036 #MJ PAR m-2 h-1 eqn 12 from paper
    ) %>% 
    summarise(absPAR = (sum(MJ*delta_t)*la))
}

combining_results_ft <- function(sim_name) {
  df <- combinations %>% 
    filter(name == sim_name) %>% 
    bind_cols(results_PAR_ft[[sim_name]])
}

combining_results_ppa <- function(sim_name) {
  df <- combinations %>% 
    filter(name == sim_name) %>% 
    bind_cols(results_PAR_ppa[[sim_name]])
}

organising_results <- function(data) {
  df <- as.data.frame(matrix(unlist(data), nrow=length(unlist(data[1])))) 
  row.names(df) <- c("H", "V", "L", "F", "S", "path", "name", "absPAR")
  df <- t(df)
  row.names(df) <- 1:nrow(combinations)
  df <- df %>% as_tibble() %>% 
    select(H, V, L, F, S, path, absPAR) %>% 
    mutate(
      name = path %>% basename() %>% gsub("_S[1-3]", "",., perl = TRUE)
    )
  df <-  df %>% 
    transform(absPAR = as.numeric(absPAR), F = as.numeric(F), 
                  H = as.numeric(H), V = as.numeric(V),
                  L = as.numeric(L), S = as.numeric(S))
  df <- df %>% 
    group_by(H, V, L, F, name) %>% 
    summarise_at(vars(absPAR), mean)
}


results[[1000]] %>% 
  mutate(
    t_h = htcrown + httrunk
  )

h <- seq(0, 50, by=100)
E <- 0*h

leaf area from ht_trunk to ht_crown

# Crown shapes

if(crownshape == "ellipsoid")

  radx <- rady <- 2.54
  htcrown <- 6.36
  httrunk <- 3
  larea <- 47.62
  
  
  deep_leaf_distribtuion <- function() {
  # adpaytioan from Mawrap, https://github.com/RemkoDuursma/Maeswrap/blob/master/R/coord3dshape.R
  n_slices <- 50
  z <- seq(0, 1, length.out = n_slices)
  zabs <- z*htcrown
  distfun <- sqrt(1 - ((z-1/2)^2)/((1/2)^2))
  rx <- radx*distfun
  ry <- rady*distfun
  slice_xs_area <- 3.14*rx*ry
  slice_xs_vol <- (htcrown/n_slices)*slice_xs_area
  slice_volume_frac <- slice_xs_vol/sum(slice_xs_vol)
  
  tibble(
    h = httrunk + zabs,
    larea = larea*slice_volume_frac
    )
  }
  
  apply to every tree
  mutate(depth = pmap(deep_leaf_distribtuion, ))
  
  
  leaf_
  
  r <- CW/2
x <- x0 + r*distfun*cos(angs)
y <- y0 + r*distfun*sin(angs)
z <- z0 + z*CL









