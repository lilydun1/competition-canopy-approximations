library(data.table)
library(curl)
library(zip)
library(tidyverse)
library(CoordinateCleaner)

hr <- readhrflux(filename = "hrflux.dat")
library(Maeswrap)
library(tidyverse)

met <- readmet(filename = "met.dat", nlines = -1)

met <- met %>% 
  filter(TIME == 1)


system("../src/maespa.out")
