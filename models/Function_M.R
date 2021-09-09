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

model_simp <- function(data) {
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

PAR_calculator_ft <- function(data, indi_la = 47.62) {
  # constant to converts ymol/s to M J / h
  UMOLperStoMJperH <-
    3600 /  # s / hr
    4.57 /  # umol quanta / J
    10^6    # J / MJ
  L_ft <- data %>% 
    filter(focal == "TRUE") %>% 
    pull(lai) 
  # load met and convert PAR to J/hr
  met <- 
    readmet(filename = "met.dat", nlines = -1) %>% 
    as_tibble() %>% 
    select(time = TIME, PAR) %>%
    mutate(      
      PAR = 0.77 * PAR * exp(-0.77*L_ft),
      MJ_per_H = PAR  * UMOLperStoMJperH  # MJ
    )
  # integrate over the day using trapezoidal integration
  pracma::trapz(met$time, met$MJ_per_H) * indi_la %>% 
    as_tibble() %>% 
    rename(absPAR = value)
}

PAR_calculator_ppa <- function(data, indi_la = 47.62) {
  # constant to converts ymol/s to M J / h
  UMOLperStoMJperH <-
    3600 /  # s / hr
    4.57 /  # umol quanta / J
    10^6    # J / MJ
  L_ft = data %>% 
    filter(focal == "TRUE") %>% 
    pull(lai_ppa) 
  # load met and convert PAR to J/hr
  met <- 
    readmet(filename = "met.dat", nlines = -1) %>% 
    as_tibble() %>% 
    select(time = TIME, PAR) %>%
    mutate(      
      PAR = 0.77 * PAR * exp(-0.77*L_ft),
      MJ_per_H = PAR  * UMOLperStoMJperH  # MJ
    )
  # integrate over the day using trapezoidal integration
  df <- pracma::trapz(met$time, met$MJ_per_H) * indi_la %>% 
    as_tibble() %>% 
    rename(absPAR = value)
}

organising_results <- function(data) {
  df <- plyr::ldply(data, data.frame) %>% 
    bind_cols(combinations_new) %>% 
    select(H, V, L, F, fla, S, name, absPAR)  %>% 
    mutate(
      name = name %>% gsub("_S[1-3]", "",., perl = TRUE)
    ) %>% 
    group_by(H, V, L, F, fla, name) %>% 
    summarise_at(vars(absPAR), mean)
}

deep_leaf_distribtuion <- function(httrunk, htcrown = 6.36, radx = 2.54, rady = 2.54, 
                                   larea = 47.62, n_slices = 50) {
  # adaption from Maeswrap, https://github.com/RemkoDuursma/Maeswrap/blob/master/R/coord3dshape.R
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
  
deep_crown_set_up <- function(d) {
  deep_crown_distribution <- 
    d$httrunk[1:nrow(d)] %>% 
    purrr::map(deep_leaf_distribtuion) 
  df <- tibble(
    tree_num = rep(1:length(deep_crown_distribution), times = 50)) %>% 
    arrange(tree_num)
  data <- plyr::ldply(deep_crown_distribution, data.frame) %>% 
    cbind(df) %>% 
    arrange(desc(h)) %>% 
    mutate(la = cumsum(larea)/10204)
  if(nrow(data) == 100*50) {
    data <- data %>% 
      filter(tree_num == 55)
  } else if(nrow(data) == 289*50) {
    data <- data %>% 
      filter(tree_num == 145)
  } else if(nrow(data) == 625*50) {
    data <- data %>% 
      filter(tree_num == 313)
  } else if(nrow(data) == 961*50) {
    data <- data %>% 
      filter(tree_num == 481)
  } else if(nrow(data) == 1156*50) {
    data <- data %>% 
      filter(tree_num == 595)
  }
}

deep_crown <- results %>% 
  purrr::map(deep_crown_set_up) 


