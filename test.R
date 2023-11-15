# init e dati ####
{
  library(sf)
  library(dplyr)
  # library(tidyverse)
  library(ggplot2)
  # library(plotly)
  # library(terra)
  # library(ncdf4) # package for netcdf manipulation
  # library(raster) # package for raster manipulation
  # library(rgdal) # package for geospatial analysis
  library(chron)
  library(readxl)
  library(glue)
  library(lubridate)
  library(readr)
  # library(mapview)
  library(purrr)
  library(stringr)
  library(mgcv)
  library(gratia)
  
  outdir <- "~/R/terni/data/dataframes"
} 

df_finale <- read_csv("data/dataframes/df_finale.csv")
names(df_finale)
# attach(df_finale)

vars <- c("t2m_median", "wspeed_mean", "pbl12_mean", "tp_mean", "cold_area", "hot_area", "scrapyard", "min_d", "ml_200", "imp_200", "bh_200", "kndvi")

mod <- lapply(vars, function(x) paste0("s(", x, ")") ) %>% paste(collapse = " + ") 

ms <- paste("gam(Cr_i ~ ", mod, ", gamma=1.4, family=gaussian(link=identity), data = df_finale)")

m <- eval(parse(text = ms)) # gam(Cr_i ~  s(t2m_median) + s(wspeed_mean) + s(pbl12_mean) + s(tp_mean) + s(cold_area) + s(hot_area) + s(scrapyard) + s(min_d) + s(ml_200) + s(imp_200) + s(bh_200) + s(kndvi) , gamma=1.4, family=gaussian(link=identity), data = df_finale)

summary(m)
appraise(m)
draw(m)

summary(m)$s.table %>% 
  as.data.frame() %>% 
  filter(`p-value` < 0.01) %>% 
  rownames() 

# secondo test ####

vars2 <- c("pbl00_IQR", "cold_area",  "scrapyard", "ml_200", "imp_200", "bh_200", "wspeed_max_max", "tmin2m_IQR", "pbl00_max")

mod2 <- lapply(vars2, function(x) paste0("s(", x, ")") ) %>% paste(collapse = " + ") 

ms2 <- paste("gam(Cr_i ~ ", mod2, ", gamma=1.4, family=gaussian(link=identity), data = df_finale)")

m2 <- eval(parse(text = ms2))

summary(m2)
appraise(m2)
draw(m2)

# terzo test ####

vars3 <- c("pbl00_IQR", "cold_area", "ml_200", "imp_200", "bh_200", "wspeed_max_max", "tmin2m_IQR", "pbl00_max", "nirradiance_IQR")

mod3 <- lapply(vars3, function(x) paste0("s(", x, ")") ) %>% paste(collapse = " + ") 

ms3 <- paste("gam(Cr_i ~ ", mod3, ", gamma=1.4, family=gaussian(link=identity), data = df_finale)")

m3 <- eval(parse(text = ms3))

summary(m3)
appraise(m3)
draw(m3)

# quarto test ####

v_variabili4 <- c(
  "t2m_mean",
  "tp_mean",
  "ptp_mean",
  "rh_mean",
  "wspeed_mean",
  "pwspeed_mean",
  "sp_mean",
  "pbl00_mean",
  "pbl12_mean",
  "kndvi",
  "imp_200",
  "bh_200",
  "cold_area",
  "hot_area",
  "scrapyard"
)

mod4 <- lapply(v_variabili4, function(x) paste0("s(", x, ")") ) %>% paste(collapse = " + ") 

ms4 <- paste("gam(value ~ ", mod4, ", gamma=1.4, family=gaussian(link=identity), data = df)")

m4 <- eval(parse(text = ms4))

summary(m4)
appraise(m4)
draw(m4)

summary(m4)$s.table %>% 
  as.data.frame() %>% 
  filter(`p-value` < 0.05) %>% 
  rownames() -> v_sign

lapply(v_sign, function(x) paste0(x) ) %>% paste(collapse = " + ") -> ms5

ms5 <- paste("gam(value ~ ", ms5, ", gamma=1.4, family=gaussian(link=identity), data = df)")

m5 <- eval(parse(text = ms5))

summary(m5)
appraise(m5)
draw(m5)

summary(m5)$s.table %>% 
  as.data.frame() %>% 
  filter(`p-value` < 0.05) %>% 
  rownames() -> v_sign5

lapply(v_sign5, function(x) paste0(x) ) %>% paste(collapse = " + ") -> ms6

ms6 <- paste("gam(value ~ ", ms6, ", gamma=1.4, family=gaussian(link=identity), data = df)")

m6 <- eval(parse(text = ms6))

summary(m6)
appraise(m6)
draw(m6)