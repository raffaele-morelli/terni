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

# SET tracciante ####
pltnt <- "Cs_i"

df <- readr::read_csv("~/R/terni/data/dataframes/df_finale_raw_lod.csv", show_col_types = FALSE)

select(df, month_y, Cs_i, rh_max, tp_IQR, imp_200, cold_area, pwspeed_mean) %>% 
  group_by(month_y) %>% 
  summarise_at(c( "rh_max", "tp_IQR", "pwspeed_mean"), unique) 

modelli <- readRDS("~/R/terni/rds_out/modelli_gaussian_clean.RDS")

index <- grep(pltnt, names(df), value = FALSE)
names(df)[index] <- "value"


dist <- 100
res <- 100

gam_tdf <- mgcv::gam(formula(modelli[[pltnt]]), data = df, gamma = 1.4, family = family(modelli[[pltnt]]))


select(df, data, value, rh_max, tp_IQR, imp_200, cold_area, pwspeed_mean) 

rh_max <- 75.2
tp_IQR <- 0.2
pwspeed_mean <- 1.15
cold_area <- 1500
imp_200 <- 50

vars <- c("rh_max", "tp_IQR", "imp_200", "cold_area", "pwspeed_mean")

# sink(glue("~/R/terni/appoggio_{pltnt}.txt"))
predict.gam(gam_tdf, data.frame(rh_max, tp_IQR, 0, cold_area, pwspeed_mean) %>% setNames(vars), type = "terms") %>% print()
predict.gam(gam_tdf, data.frame(rh_max, tp_IQR, 50, cold_area, pwspeed_mean) %>% setNames(vars), type = "terms") %>% print()
predict.gam(gam_tdf, data.frame(rh_max, tp_IQR, 100, cold_area, pwspeed_mean) %>% setNames(vars), type = "terms") %>% print()

predict.gam(gam_tdf, data.frame(rh_max, tp_IQR, 0, cold_area, pwspeed_mean) %>% setNames(vars), type = "response") %>% print()
predict.gam(gam_tdf, data.frame(rh_max, tp_IQR, 50, cold_area, pwspeed_mean) %>% setNames(vars), type = "response")%>% print()
predict.gam(gam_tdf, data.frame(rh_max, tp_IQR, 100,cold_area, pwspeed_mean) %>% setNames(vars), type = "response")%>% print()

predict.gam(gam_tdf, data.frame(rh_max, tp_IQR, 0, cold_area, pwspeed_mean) %>% setNames(vars), type = "lpmatrix")%>% print()
predict.gam(gam_tdf, data.frame(rh_max, tp_IQR, 50, cold_area, pwspeed_mean) %>% setNames(vars), type = "lpmatrix")%>% print()
predict.gam(gam_tdf, data.frame(rh_max, tp_IQR, 100, cold_area, pwspeed_mean) %>% setNames(vars), type = "lpmatrix")%>% print()


rbind(c(50, tp_IQR, imp_200, cold_area, pwspeed_mean),
      c(75, tp_IQR, imp_200, cold_area, pwspeed_mean),
      c(100, tp_IQR, imp_200, cold_area, pwspeed_mean)
) %>% as.data.frame() %>% setNames(vars) -> df_p

df_p %>%  print()

predict.gam(gam_tdf, df_p, type = "response") %>% print()

# sink()

