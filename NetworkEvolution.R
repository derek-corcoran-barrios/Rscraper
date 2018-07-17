if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sqldf, igraph)

NetworkCran <- read_rds("2018_06_23NetworkCran.rds")
pageDate <- read_rds("pageDateComplete.rds")

NetworkDate <- NetworkCran %>% left_join(pageDate) %>% dplyr::select(Package, Depends, First_date) %>% arrange(First_date)
