create_perimeter <- function(nt = n, sqn = sn){
  sqn_seq <- seq.int(from = sqn*2, to = nt-sqn*2, by = nt/sqn)
  sqn_one_seq <- sqn_seq + 1
  sqn_two_seq <- sqn_seq + 2
  sqn_three_seq <- sqn_seq - 1
  perimeter_trees <- c(1:(2*sqn-2), sqn_seq, sqn_one_seq, sqn_two_seq, sqn_three_seq, (nt-(2*sqn-3)):(nt)) %>% 
    subset(., . <= nt) %>% 
    sort()
}

SD_height <- function(n = ntrees, h_mn, h_sd, sn = sqrt_n) {
  x <- 
    rnorm(1000000, mean = h_mn, sd = h_sd) %>% 
    subset(., . >=0)
  x <- x[1:n] %>% 
    round(digits = 2) %>% 
    as.data.frame()
  perimeter_trees <- create_perimeter(nt = n, sqn = sn)
  x <- replace(x$., perimeter_trees, 0.001) %>% 
    as.data.frame()
}

replace_perimeter_h <- function(n = ntrees, sn = sqrt_n, crown_h = indivhtcrown, trunk_h = h_mn) {
  perimeter_trees <- create_perimeter(nt = n, sqn = sn)
  crown_height <- rep(crown_h, n) %>%
    replace(perimeter_trees, values = (trunk_h + crown_h)) %>% 
    as.matrix()
}

replace_perimeter_r <- function(n = ntrees, sn = sqrt_n, crown_r = indivrad) {
  perimeter_trees <- create_perimeter(nt = n, sqn = sn)
  crown_height <- rep(crown_r, n) %>%
    replace(perimeter_trees, values = 13) %>% 
    as.matrix()
}

replace_perimeter_la <- function(n = ntrees, sn = sqrt_n, crown_la = indivlarea) {
  perimeter_trees <- create_perimeter(nt = n, sqn = sn)
  la_perimeter <- rep(crown_la, n) %>%
    replace(perimeter_trees, values = 900) %>% 
    as.matrix()
}

create_coordinates <- function(sn = sqrt_n) {
  list1 <- seq(0, 196, length.out = sn) %>% 
    round(digits = 2)
  df = data.frame()
  df_matrix <- expand.grid(x = list1, y = list1) %>% 
    arrange(x, y) %>% 
    as.matrix()
}

create_trees <- function(path, h_mn = 10, h_cv = 0.1, LAI = 1.54, ft_h = 1.99, 
                         plot_area = 20000, indivlarea = 47.62, indivrad = 5.087809/2, 
                         indivhtcrown = 6.359762, indivdiam = 0.3, seed = 6, half = 1/2) {
  wd <- setwd(path)
  on.exit(setwd(wd))
  set.seed(seed)
  h_sd <- h_mn * h_cv
  ntrees <- ceiling(plot_area*(LAI/indivlarea))
  sqrt_n <- ceiling(sqrt(ntrees))
  SD_height_df <- SD_height(n = ntrees, h_mn, h_sd, sn = sqrt_n) 
  coordinates <- create_coordinates(sqrt_n)
  ft <- floor((sqrt_n*floor(sqrt_n*half)) + ceiling(half*sqrt_n))
  SD_height_df <- replace(SD_height_df$., ft, h_mn*ft_h)
  perimeter_h <- replace_perimeter_h(ntrees, sqrt_n, indivhtcrown, h_mn)
  perimeter_r <- replace_perimeter_r(ntrees, sqrt_n, indivrad)
  perimeter_la <- replace_perimeter_la(ntrees, sqrt_n, indivlarea)
  replacePAR("confile.dat", "itargets", "treescon", newval = ft)
  replacePAR("confile.dat", "notrees", "treescon", newval = ntrees)
  replacePAR("Trees.dat", "xycoords", "xy", newval= coordinates)
  replacePAR("Trees.dat", "values", "indivhttrunk", newval = SD_height_df)
  replacePAR("Trees.dat", "notrees", "plot", newval = ntrees)
  replacePAR("Trees.dat", "values", "indivradx", newval = perimeter_r)
  replacePAR("Trees.dat", "values", "indivrady", newval = perimeter_r)
  replacePAR("Trees.dat", "values", "indivhtcrown", newval = perimeter_h)
  replacePAR("Trees.dat", "values", "indivdiam", newval = rep(indivdiam, ntrees))
  replacePAR("Trees.dat", "values", "indivlarea", newval = perimeter_la)
}

create_simulation <- function(path = "simulations/Trail", template = "template_D",...) {
  unlink(path, recursive = T)
  dir.create(path, showWarnings = F, recursive = T)
  list.files(template, full.names = T)
  file.copy(list.files(template, full.names = T), path)
  create_trees(path,...)
}

run_simulation <- function(path) {
  wd <- setwd(path)
  on.exit(setwd(wd))
  n <- stringr::str_count(path,"/")
  maespa <- paste0(c(rep("..", n+1), "src/maespa.out"), collapse = "/")
  system(maespa)
}

load_output <- function(path){
  readdayflux(filename = file.path(path,"dayflx.dat")) %>% 
    as_tibble() %>% 
    mutate(path = path)
}
