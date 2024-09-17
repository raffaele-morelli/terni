{
  library(readr)
  library(dplyr)
  library(logr)
  library(purrr)
  library(mgcv)
  library(stringr)
  library(correlation)
  library(readxl)
  library(glue)
  
  setwd("~/R/terni")
  
  # df <- read_csv(glue::glue("data/dataframes/df_finale_lod_clean.csv"), show_col_types = FALSE)
  df <- read_csv(glue::glue("~/R/terni/data/dataframes/df_finale_raw.csv"), show_col_types = FALSE)
  traccianti <- readRDS("~/R/terni/rds_out/traccianti.RDS")
}

map(traccianti[1:71], \(t) {
  # hist(log(df[[t]]), breaks = 100, main = t)
  cat(t, "\n")
  shapiro.test( log( na.omit(df[[t]])) ) 
}) -> test

do.call(rbind.data.frame, test) %>% 
  as.data.frame() %>% 
  select(c(1,2)) %>% 
  cbind(traccianti[1:71]) -> test


walk(traccianti["Na_i"], \(t) {
  print(t)
  EnvStats::qqPlot(df[[t]], distribution = "gamma", estimate.params = TRUE, main = t)
})
