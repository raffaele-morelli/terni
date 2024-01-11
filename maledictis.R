library(sf)
library(dplyr)
library(ggplot2)
library(terra)
library(purrr)
library(stringr)
library(logr)
library(lubridate)
library(glue)
library(utils)
library(mgcv)

df <- readr::read_csv("~/R/terni/data/dataframes/df_finale_raw_lod.csv", show_col_types = FALSE)

select(df, month_y, Cs_i, rh_max, tp_IQR, imp_200, cold_area) %>% 
  group_by(month_y) %>% 
  summarise_at(c( "rh_max", "tp_IQR"), unique) 

modelli <- readRDS("~/R/terni/rds_out/modelli_gaussian_clean.RDS")

index <- grep(pltnt, names(df), value = FALSE)
names(df)[index] <- "value"



# SET tracciante ####
pltnt <- "Cs_i"

dist <- 100
res <- 100

modelli <- readRDS("~/R/terni/rds_out/modelli_gaussian_clean.RDS")

gam_tdf <- mgcv::gam(formula(modelli[[pltnt]]), data = df, gamma = 1.4, family = family(modelli[[pltnt]]))

select(df, data, value, rh_max, tp_IQR, imp_200, cold_area) 

rh_max <- 10
tp_IQR <- 7.30
vars <- c("rh_max", "tp_IQR", "imp_200", "cold_area")

sink("appoggio.txt")
predict.gam(gam_tdf, data.frame(rh_max, tp_IQR, 0, 2000) %>% setNames(vars), type = "terms")
predict.gam(gam_tdf, data.frame(rh_max, tp_IQR, 50, 2000) %>% setNames(vars), type = "terms")
predict.gam(gam_tdf, data.frame(rh_max, tp_IQR, 100, 2000) %>% setNames(vars), type = "terms")

predict.gam(gam_tdf, data.frame(rh_max, tp_IQR, 0, 2000) %>% setNames(vars), type = "response") 
predict.gam(gam_tdf, data.frame(rh_max, tp_IQR, 50, 2000) %>% setNames(vars), type = "response")
predict.gam(gam_tdf, data.frame(rh_max, tp_IQR, 100, 2000) %>% setNames(vars), type = "response")

predict.gam(gam_tdf, data.frame(rh_max, tp_IQR, 0, 2000) %>% setNames(vars), type = "lpmatrix")
predict.gam(gam_tdf, data.frame(rh_max, tp_IQR, 50, 2000) %>% setNames(vars), type = "lpmatrix")
predict.gam(gam_tdf, data.frame(rh_max, tp_IQR, 100, 2000) %>% setNames(vars), type = "lpmatrix")

sink()

rbind(c(rh_max, tp_IQR, 0, 2000),
      c(rh_max, tp_IQR, 50, 2000),
      c(rh_max, tp_IQR, 100, 2000)
) %>% as.data.frame() %>%  setNames(vars) -> df_p

predict.gam(gam_tdf, df_p, type = "response")
