load_trees <- function(path) {
  trees <- parseFile(file.path(path,"trees.dat"))
  data <- tibble(tree_num = seq_len(length(trees$indivradx$values)),
                radx = trees$indivradx$values, rady =  trees$indivrady$values,  htcrown = trees$indivhtcrown$values, 
                 diam = trees$indivdiam$values,  httrunk = trees$indivhttrunk$values, larea = trees$indivlarea$values, 
                 x = trees$xy$xycoords[seq(1, length(trees$xy$xycoords), by = 2)], 
                 y = trees$xy$xycoords[seq(2, length(trees$xy$xycoords), by = 2)])
  data <- data %>% 
    filter(x > 27.999, x < 168.001, y > 27.999, y < 168.001)
}

mark_focal_tree <- function(data) {
    if(nrow(data) == 100) {
      data <- data %>% add_column(focal = ifelse(data$x == 105.54 & data$y == 90.46, TRUE, FALSE))
    } else if(nrow(data) == 324) {
      data <- data %>% add_column(focal = ifelse(data$x == 101.92 & data$y == 94.08, TRUE, FALSE))
    } else if(nrow(data) == 676) {
      data <- data %>% add_column(focal = ifelse(data$x == 100.8 & data$y == 95.2, TRUE, FALSE))
    } else if(nrow(data) != 100 & nrow(data) != 324 & nrow(data) != 676) {
      data <- data %>% add_column(focal = ifelse(data$x == 98 & data$y == 98, TRUE, FALSE))
    }
  data
}

model_lai <- function(data) {
  
  # flat top
  ft <- 
    data %>% 
    mutate(t_h = htcrown + httrunk + rnorm(length(htcrown), 0, 0.01)) %>% 
    arrange(desc(t_h)) %>% 
    group_by(t_h, tree_num, focal) %>%
    summarise(larea = sum(larea)) %>% 
    ungroup() %>% 
    arrange(desc(t_h)) %>% 
    mutate(
      h=t_h,
      lai = cumsum(larea)/6123,
      model = "FLAT TOP"
    ) %>%
    filter(focal == "TRUE") %>% 
    as_tibble()
  
  ## perfect plasticity
  ppa <-
    ft %>%
    mutate(
    lai = floor(lai),
    model = "PPA"
    )
  
  # deep crown
  dc <- data %>% deep_crown_set_up()
  
  bind_rows(ft, ppa, dc) %>% 
    select(model, tree_num, focal, t_h, h, larea, lai)
}

PAR_calculator <- function(lai, stream, met) {
  # constant to converts ymol/s to M J / h
  UMOLperStoMJperH <-
    3600 /  # s / hr
    4.57 /  # umol quanta / J
    10^6    # J / MJ

  met$lai <- lai
  
  if(stream == 1) {
    met$PAR_L = 0.77 * met$PAR * exp(-0.77*lai)
  } else  if(stream == 2) {
    met$PAR_L = 0.77*met$PAR*(exp(-0.77*lai) + (1- exp(-0.77*lai))*exp(-0.35*lai))
  }
  
  met$MJ_per_H = met$PAR_L * UMOLperStoMJperH # MJ
  
  met
}

deep_leaf_distribtuion <- function(httrunk, htcrown = 6.36, radx = 2.54, rady = 2.54, 
                                   larea = 27, n_slices = 50) {
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
    slice = seq_len(length(z)),
    h = httrunk + zabs,
    larea_slice = larea*slice_volume_frac
    )
}

deep_crown_set_up <- function(d) {
  d %>% 
    mutate(
      t_h = httrunk + htcrown,
      leaf_dist = purrr::pmap(list(httrunk, htcrown, radx, rady, larea), deep_leaf_distribtuion)
    ) %>% 
    unnest(leaf_dist) %>%
    arrange(desc(h)) %>% 
    mutate(lai = cumsum(larea_slice)/6123,
           model = "DEEP CROWN") %>% 
    filter(focal==TRUE) %>%
    select(t_h, tree_num, h, focal, larea = larea_slice, lai, model)
}

#stand functions
focal_tree_stand <- function(data) {
  if(nrow(data) != 961){
    data <- data %>% add_column(focal = 
                   ifelse(data$x > 75.37 & data$x < 120.63 & data$y > 75.37 & data$y < 120.63, 
                          TRUE, FALSE))
  } else if(nrow(data) == 961) {
      data <- data %>% add_column(focal = 
                     ifelse(data$x > 75.72 & data$x < 115.83  & data$y > 75.72 & data$y < 115.83, 
                            TRUE, FALSE))
  }
}

met_f <- function(L_ft) {
  met <- 
    readmet(filename = "met.dat", nlines = -1) %>% 
    as_tibble() %>% 
    select(time = TIME, PAR) %>%
    mutate(
      PAR_one_s = 0.77 * PAR * exp(-0.77*L_ft), #one stream
      PAR_two_s = 0.77*PAR*(exp(-0.77*L_ft) + (1- exp(-0.77*L_ft))*exp(-0.35*L_ft)), #two stream 
      MJ_per_H_one_s = PAR_one_s * UMOLperStoMJperH, 
      MJ_per_H_two_s = PAR_two_s * UMOLperStoMJperH  # MJ
    )
}

PAR_calculator_ft_stand <- function(data, indi_la = 1) {
  # constant to converts ymol/s to M J / h
  UMOLperStoMJperH <-
    3600 /  # s / hr
    4.57 /  # umol quanta / J
    10^6    # J / MJ
  L_ft <- data %>% 
    pull(lai) 
  # load met and convert PAR to J/hr
  trying <- L_ft %>% 
    map(met_f)
  # integrate over the day using trapezoidal integration
  for(i in 1:length(trying)){
    trying[[i]] <- tibble(absPAR_one_s = pracma::trapz(trying[[i]]$time, trying[[i]]$MJ_per_H_one_s) * indi_la, 
                          absPAR_two_s = pracma::trapz(trying[[i]]$time, trying[[i]]$MJ_per_H_two_s) * indi_la)
  }
  df <- plyr::ldply(trying, data.frame) %>% 
    mutate(absPAR_one_s = absPAR_one_s*0.1, 
           absPAR_two_s = absPAR_two_s*0.1) %>% 
    summarise(absPAR_one_s = sum(absPAR_one_s), 
              absPAR_two_s = sum(absPAR_two_s))
}

PAR_calculator_ppa_stand <- function(data, indi_la = 1) {
  # constant to converts ymol/s to M J / h
  UMOLperStoMJperH <-
    3600 /  # s / hr
    4.57 /  # umol quanta / J
    10^6    # J / MJ
  L_ft <- data %>% 
    pull(lai_ppa) 
  # load met and convert PAR to J/hr
  trying <- L_ft %>% 
    map(met_f)
  # integrate over the day using trapezoidal integration
  for(i in 1:length(trying)){
    trying[[i]] <- tibble(absPAR_one_s = pracma::trapz(trying[[i]]$time, trying[[i]]$MJ_per_H_one_s) * indi_la, 
                          absPAR_two_s = pracma::trapz(trying[[i]]$time, trying[[i]]$MJ_per_H_two_s) * indi_la)
  }
  df <- plyr::ldply(trying, data.frame) %>%    
    mutate(absPAR_one_s = absPAR_one_s*0.1, 
           absPAR_two_s = absPAR_two_s*0.1) %>% 
    summarise(absPAR_one_s = sum(absPAR_one_s), 
              absPAR_two_s = sum(absPAR_two_s))
}

deep_crown_set_up_stand <- function(d) {
  LAI0.5_focal = c(34:37, 44:47, 54:57, 64:67)
  LAI1.5_focal = c(115:120, 133:138 , 151:156 , 170:175 , 187:192 , 205:210)
  LAI3_focal = c(244:251, 270:277, 296:303, 322:329, 348:355, 374:381, 
                 400:407, 426:433)
  LAI4.5_focal = c(321:330, 352:361, 383:392, 414:423, 445:454, 476:485, 
                   507:516, 538:547, 569:578, 600:609)
  LAI5.5_focal = c(433:443, 468:478, 503:513, 538:548, 573:583, 608:618, 
                   643:653, 678:688, 713:723, 748:758, 783:793)
  deep_crown_distribution <- 
    d$httrunk[1:nrow(d)] %>% 
    purrr::map(deep_leaf_distribtuion) 
  df <- tibble(
    tree_num = rep(1:length(deep_crown_distribution), times = 50)) %>% 
    arrange(tree_num)
  data <- plyr::ldply(deep_crown_distribution, data.frame) %>% 
    cbind(df) %>% 
    arrange(desc(h)) %>% 
    mutate(la = cumsum(larea)/6123)
  if(nrow(data) == 100*50) {
    data <- data %>% 
      filter(tree_num %in% LAI0.5_focal)
  } else if(nrow(data) == 324*50) {
    data <- data %>% 
      filter(tree_num %in% LAI1.5_focal)
  } else if(nrow(data) == 676*50) {
    data <- data %>% 
      filter(tree_num %in% LAI3_focal)
  } else if(nrow(data) == 961*50) {
    data <- data %>% 
      filter(tree_num %in% LAI4.5_focal)
  } else if(nrow(data) == 1225*50) {
    data <- data %>% 
      filter(tree_num %in% LAI5.5_focal) 
  }
}

PAR_calculator_DC_stand <- function(la) {
  UMOLperStoMJperH <-
    3600 /  # s / hr
    4.57 /  # umol quanta / J
    10^6    # J / MJ
  met <- readmet(filename = "met.dat", nlines = -1) %>% 
    as_tibble() %>% 
    select(time = TIME, PAR) %>% 
    mutate(  
      PAR_one_s = 0.77 * PAR * exp(-0.77*la),
      PAR_two_s = 0.77* PAR*(exp(-0.77*la) + (1- exp(-0.77*la))*exp(-0.35*la)), 
      MJ_per_H_one_s = PAR_one_s * UMOLperStoMJperH, 
      MJ_per_H_two_s = PAR_two_s * UMOLperStoMJperH )
  tibble(absPAR_one_s = pracma::trapz(met$time, met$MJ_per_H_one_s) * 0.1, 
         absPAR_two_s = pracma::trapz(met$time, met$MJ_per_H_two_s) * 0.1) 
}

applying_DC_stand <- function(data) {
  data$la[1:nrow(data)] %>% 
    map_df(PAR_calculator_DC_stand)
}

summarise_DC_stand <- function(data) {
  df <- tibble(tree = rep(1:(nrow(data)/50), times = 50)) %>% 
    arrange(tree)
  d <- cbind(data, df) 
  da <- d %>% group_by(tree) %>% 
    summarise(absPAR_one_s = mean(absPAR_one_s), 
              absPAR_two_s = mean(absPAR_two_s)) %>% 
    select(absPAR_one_s, absPAR_two_s) %>% 
    summarise(absPAR_one_s = sum(absPAR_one_s), 
              absPAR_two_s = sum(absPAR_two_s))
}


