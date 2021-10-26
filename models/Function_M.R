load_maespa_results <- function(path, per_leaf_area = TRUE) {
  results <- read_csv(path) %>%
    select(H, V, L, F, fla, name, 
           absPAR_one_s = absPAR,
           absPAR_two_s = absPAR
           # NB this is not a mistake! 
           # maespa doesn't have one stream model, so use two stream for comparison
           ) %>% 
    add_column(model = "MAESPA")
  
  if(per_leaf_area)
    results <- results %>%
      mutate(
        absPAR_one_s = absPAR_one_s/fla,
        absPAR_two_s = absPAR_two_s/fla
    )
  
  results
}

process_experiment <- function(path, H, V, L, F, fla, S, stand = FALSE, max_n =1e4) {
  
  # load met forcing
  met <-  
    readmet(filename = "met.dat", nlines = -1) %>% 
    as_tibble() %>% 
    select(time = TIME, PAR)
  
  # Choose function for marking focal trees
  if(stand) mark_focal_tree <- mark_focal_tree_stand
  else mark_focal_tree <- mark_focal_tree_single
  
  combinations <- 
    expand_grid(H, V, L, F, fla, S) %>% 
    mutate(path = sprintf("%s/H%s_V%s_L%s_F%s_fla%s_S%s", path, H, V, L, F, fla, S)) %>% 
    add_column(name = basename(.$path))  %>% 
    slice(1:max_n) %>%
    mutate(trees = map(path, load_trees), 
           f_trees = map(trees, mark_focal_tree))
  
  results <- 
    combinations %>%
    mutate(
      models_lai = map(f_trees, model_lai)
    ) %>%
    unnest(models_lai) %>%
    # calculate light PAR
    mutate(
      PAR_one_s = map(lai, PAR_calculator, stream = 1, met = met),
      PAR_two_s = map(lai, PAR_calculator, stream = 2, met = met),
      # integrate over the day using trapezoidal integration
      absPAR_one_s = map_dbl(PAR_one_s, ~pracma::trapz(.x$time, .x$MJ_per_H)),
      absPAR_two_s = map_dbl(PAR_two_s, ~pracma::trapz(.x$time, .x$MJ_per_H))
    )
  results
}

calculate_averages <- function(results, per_leaf_area = TRUE) {

  # Calculate totals for each plant in each simulation
  out <-
    results %>%
    # sum over leaf segments (needed for deep crown, others only have 1 segment)
    group_by(H, V, L, F, fla, S, name, model) %>%
    summarise(
      t_h = mean(t_h),
      absPAR_one_s = sum(absPAR_one_s*larea), 
      absPAR_two_s = sum(absPAR_two_s*larea),
      larea = sum(larea)
    ) %>%
    ungroup() 
  
  if(per_leaf_area) {
    out <- out %>%
      mutate(
        absPAR_one_s = absPAR_one_s/larea, 
        absPAR_two_s = absPAR_two_s/larea
      )
  }
  
  out %>%
    # average over replicate simulations S==1, S==2...
    mutate(
      name = name %>% gsub("_S[1-3]", "",., perl = TRUE)
    ) %>% 
    group_by(H, V, L, F, fla, name, model) %>% 
    summarise_at(vars(absPAR_one_s, absPAR_two_s), mean) %>%
    ungroup()
}


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

mark_focal_tree_single <- function(data) {
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

mark_focal_tree_stand <- function(data) {
  if(nrow(data) != 961){
    data <- data %>% add_column(focal = 
                                  ifelse(data$x > 75.37 & data$x < 120.63 & data$y > 75.37 & data$y < 120.63, 
                                         TRUE, FALSE))
  } else if(nrow(data) == 961) {
    data <- data %>% add_column(focal = 
                                  ifelse(data$x > 75.72 & data$x < 115.83  & data$y > 75.72 & data$y < 115.83, 
                                         TRUE, FALSE))
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



