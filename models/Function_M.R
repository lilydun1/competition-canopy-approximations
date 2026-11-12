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
      # integrate over the day using trapezoidal integratio..n
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




## ===========================================================================
## 2026 rerun (simulations2026_*; see ../Maespa simulations/Sims_2026.R)
## ---------------------------------------------------------------------------
## Changes from the functions above, which are kept so the first-round analysis
## remains reproducible:
##   - the focal tree is read from confile.dat (itargets) rather than matched on
##     hard-coded coordinates;
##   - LAI is computed from the realised ground area per tree (inner trees x
##     spacing^2) rather than a fixed 6123 m², which did not match the geometry;
##   - PPA layers are defined by cumulative crown area (Strigul et al. 2008): a
##     tree's layer is the number of complete canopy layers (crown area = ground
##     area) formed by taller trees, and it receives the light below them;
##   - light coefficients k1, k2 are arguments (fitted to MAESPA, see
##     calibrate_light()).
## ===========================================================================

load_trees_2026 <- function(path) {
  focal <- parseFile(file.path(path, "confile.dat"))$treescon$itargets
  load_trees(path) %>% mutate(focal = tree_num == focal)
}

## ground area represented by the inner trees: n_inner x spacing^2
stand_area <- function(data) {
  nrow(data) * median(diff(sort(unique(data$x))))^2
}

model_lai_2026 <- function(data) {
  area <- stand_area(data)

  ordered <- data %>%
    mutate(t_h = htcrown + httrunk + rnorm(n(), 0, 0.01)) %>%  # jitter breaks ties
    arrange(desc(t_h)) %>%
    mutate(crown_area = pi * radx * rady,
           layer = floor((cumsum(crown_area) - crown_area) / area))
  la_by_layer <- ordered %>% group_by(layer) %>% summarise(la = sum(larea)) %>%
    arrange(layer) %>% mutate(la_above = cumsum(la) - la)

  ft <- ordered %>%
    mutate(h = t_h, lai = cumsum(larea) / area, model = "FLAT TOP") %>%
    filter(focal)
  ppa <- ordered %>%
    left_join(la_by_layer, by = "layer") %>%
    mutate(h = t_h, lai = la_above / area, model = "PPA") %>%
    filter(focal)
  dc <- data %>%
    mutate(t_h = httrunk + htcrown,
           leaf_dist = purrr::pmap(list(httrunk, htcrown, radx, rady, larea),
                                   deep_leaf_distribtuion)) %>%
    select(-larea) %>%
    unnest(leaf_dist) %>%
    arrange(desc(h)) %>%
    mutate(larea = larea_slice, lai = cumsum(larea_slice) / area, model = "DEEP CROWN") %>%
    filter(focal)

  bind_rows(ft, ppa, dc) %>%
    select(model, tree_num, focal, t_h, h, larea, lai) %>%
    mutate(area = area)
}

## LAI above the focal tree (or each Deep Crown slice) for every stand in a grid
approx_lai_2026 <- function(folder, combos) {
  combos %>%
    mutate(path = sprintf("%s/H%s_V%s_L%s_F%s_fla%s_S%s", folder, H, V, L, F, fla, S),
           lai_tab = map(path, ~ model_lai_2026(load_trees_2026(.x)))) %>%
    select(-path) %>%
    unnest(lai_tab)
}

## daily PAR integral above the canopy (MJ m-2 d-1) from met.dat
daily_par <- function(met_file = "met.dat") {
  met <- readmet(filename = met_file, nlines = -1) %>% as_tibble()
  pracma::trapz(met$TIME, met$PAR * 3600 / 4.57 / 1e6)
}

## Absorbed PAR per unit leaf area at a given LAI above.
## Two-component light model (stream = 2): a fraction w of the light attenuates
## quickly (ka) and the rest slowly (kb); a = absorptance at the canopy top.
## Single-component Beer's law (stream = 1): one extinction coefficient k.
## Because the light profile depends only on LAI above, the daily integral
## factorises into G (the day's incoming PAR) times the transmission.
absorb <- function(lai, p, G, stream = 2) {
  if (stream == 1) p[["a"]] * G * exp(-p[["k"]] * lai)
  else p[["a"]] * G * (p[["w"]] * exp(-p[["ka"]] * lai) + (1 - p[["w"]]) * exp(-p[["kb"]] * lai))
}

## sum over Deep Crown slices (leaf-area weighted), then average replicates
approx_par_2026 <- function(lai_tab, p, G, stream = 2) {
  lai_tab %>%
    mutate(par = absorb(lai, p, G, stream)) %>%
    group_by(H, V, L, F, fla, S, model) %>%
    summarise(par = sum(par * larea) / sum(larea), .groups = "drop") %>%
    group_by(H, V, L, F, fla, model) %>%
    summarise(absPAR = mean(par), .groups = "drop")
}

## Calibrate the light model against MAESPA using only focal trees at the two
## extremes -- full light (height ratio 1.99) and deep shade (ratio 0.01), all
## densities and CVs -- where the Flat Top and Deep Crown agree, so the fit does
## not favour any approximation. a is set from full light; the transmission
## parameters are then fitted by least squares to the deep-shade trees.
calibrate_light <- function(lai_tab, maespa, G, stream = 2) {
  ext <- filter(lai_tab, model == "FLAT TOP", F %in% c(0.01, 1.99))
  a <- mean(filter(maespa, F == 1.99)$absPAR) / G
  sse <- function(p) {
    pr <- approx_par_2026(ext, p, G, stream) %>%
      inner_join(maespa, by = c("H", "V", "L", "F", "fla"), suffix = c("", ".m"))
    sum((pr$absPAR - pr$absPAR.m)^2)
  }
  if (stream == 1) {
    k <- optimize(function(k) sse(c(a = a, k = k)), c(0.01, 5))$minimum
    return(c(a = a, k = k))
  }
  q <- optim(c(0, log(2), log(0.5)), function(q)
    sse(c(a = a, w = plogis(q[1]), ka = exp(q[2]), kb = exp(q[3]))))$par
  c(a = a, w = plogis(q[1]), ka = exp(q[2]), kb = exp(q[3]))
}

## ---------------------------------------------------------------------------
## Approximation curves at arbitrary focal heights (for plotting)
## ---------------------------------------------------------------------------
## Within a replicate, the stand is identical for every focal-height ratio F
## (same seed); only the focal tree's trunk height (15 m x F) changes. The
## approximations depend only on the leaf area above the focal tree, so they can
## be evaluated at any F from a single stand. Each stand is sorted once and the
## leaf area above the focal tree is then looked up for every F, reproducing
## model_lai_2026() (up to the random tie-breaking it applies to equal heights).
approx_curve_stand <- function(path, F_grid, fla, p, G, h_mn = 15) {
  d <- load_trees_2026(path)
  area <- stand_area(d)
  focal <- d[d$focal, ]
  st <- d[!d$focal, ] %>% mutate(top = httrunk + htcrown, ca = pi * radx * rady) %>% arrange(desc(top))
  st <- st %>% mutate(cum_la = cumsum(larea), cum_ca = cumsum(ca),
                      layer = floor((cum_ca - ca) / area))
  top_desc <- st$top
  ## Deep Crown: all stand slices, sorted by height, with cumulative leaf area
  sl <- st %>% mutate(s = purrr::pmap(list(httrunk, htcrown, radx, rady, larea), deep_leaf_distribtuion)) %>%
    select(s) %>% unnest(s) %>% arrange(h)
  sl_h <- sl$h; sl_above <- rev(cumsum(rev(sl$larea_slice)))   # leaf area at or above each slice
  la_above_h <- function(h) {                                     # stand leaf area strictly above h
    i <- findInterval(h, sl_h)                                    # number of slices with height <= h
    ifelse(i >= length(sl_h), 0, sl_above[i + 1])
  }
  map_dfr(F_grid, function(F) {
    trunk <- h_mn * F
    top <- trunk + focal$htcrown
    k <- sum(top_desc > top)                                      # stand trees taller than the focal
    lai_ft <- ((if (k > 0) st$cum_la[k] else 0) + fla) / area
    fl <- floor((if (k > 0) st$cum_ca[k] else 0) / area)          # focal's PPA layer
    m <- sum(st$layer < fl)
    lai_ppa <- (if (m > 0) st$cum_la[m] else 0) / area
    fs <- deep_leaf_distribtuion(trunk, focal$htcrown, focal$radx, focal$rady, fla) %>% arrange(desc(h))
    lai_dc <- (la_above_h(fs$h) + cumsum(fs$larea_slice)) / area
    tibble(F = F, model = c("FLAT TOP", "PPA", "DEEP CROWN"),
           absPAR = c(absorb(lai_ft, p, G), absorb(lai_ppa, p, G),
                      sum(absorb(lai_dc, p, G) * fs$larea_slice) / sum(fs$larea_slice)))
  })
}

## Curves for a set of stands (one stand per V, L, fla, S; built at F = 1),
## averaged over replicates
approx_curves_2026 <- function(folder, stands, F_grid, p, G) {
  stands %>%
    mutate(path = sprintf("%s/H%s_V%s_L%s_F1_fla%s_S%s", folder, H, V, L, fla, S),
           curve = purrr::pmap(list(path, fla), function(path, fla) approx_curve_stand(path, F_grid, fla, p, G))) %>%
    select(-path) %>% unnest(curve) %>%
    group_by(H, V, L, fla, F, model) %>% summarise(absPAR = mean(absPAR), .groups = "drop")
}
