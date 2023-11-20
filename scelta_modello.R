## init ####
{
  library(readr)
  library(dplyr)
  library(logr)
  library(purrr)
  library(mgcv)
  library(stringr)
  library(correlation)
  library(readxl)
  
  library(datiInquinanti)
  library(datiMeteo)
  
  setwd("~/R/terni")
  rm(list = ls())
  
  source('f_buildMods.R')
  source('f_bestMod.R')
  source('f_sceltaVar.R')
  
  pltnt <- "PM10" #### SET inquinante ####
  df <- read_csv(glue::glue("data/dataframes/df_finale_{pltnt}.csv"), show_col_types = FALSE)
  
  findCappa <- function(var) {
    df[[var]] %>% unique() %>% length()
  }
  
  map(names(df), findCappa) -> cappas
  names(cappas) <- names(df)  
}

## Variabili #####
{
  df_terni_mensili_correlazione <- read_excel("data/df_terni_mensili_correlazione.xlsx", sheet = " Variabili scelte")
  v_scelte <- df_terni_mensili_correlazione$`Variabili scelte`
  
  idxs <- c(23)
  v_scelte <- v_scelte[!v_scelte %in% v_scelte[idxs]]
  # idxs <- grep("pbl00_min|tp_median|pblmin_median", names(cappas))
  # cappas <- cappas[-idxs]
  
  v_meteo <- grep("mean", names(df), value = FALSE) # le variabili meteo (media)
  v_buf200 <- grep("200", names(df), value = TRUE)[1:4] # solo i buffer 200
  v_urban_atlas <- grep("s8_sup_200|s7_sup_200", names(df), value = TRUE)
  v_acciaieria <- c("cold_area", "hot_area", "scrapyard")
  
  v_variabili <- v_scelte
  # v_variabili <- c("t2m_IQR", "tmin2m_IQR", "tmax2m_IQR", "rh_IQR", "t2m_median" )
  # v_variabili <- c("t2m_mean", "t2m_IQR", "tmin2m_IQR", "tmax2m_IQR", "tp_max", "rh_IQR", "t2m_median" )
  # v_variabili <- c("t2m_mean", "t2m_IQR", "tmin2m_IQR", "tmax2m_IQR", "tp_max", "rh_IQR", "u10m_IQR" )
  # v_variabili <- c("t2m_mean", "t2m_IQR", "tmin2m_IQR", "tmax2m_IQR", "tp_max", "rh_IQR", "u10m_IQR", v_buf200 )
  # v_variabili <- c("t2m_mean", "t2m_IQR", "tmin2m_IQR", "tmax2m_IQR", "tp_max", "rh_IQR", "u10m_IQR", v_buf200, v_acciaieria)
  # v_variabili <- c(v_scelte)
  v_variabili <- c(v_scelte, v_buf200, v_acciaieria, v_urban_atlas)
  
}

# Variabili "ambiente" ####
assign("v_variabili", v_variabili, envir = .GlobalEnv)
assign("AICS", list(), envir = .GlobalEnv)
assign("v_dead", c(), envir = .GlobalEnv)
assign("N", 0, envir = .GlobalEnv)
assign("cappas", cappas, envir = .GlobalEnv)

fn <- file.path("stazione.log")
lf <- log_open(fn)

# funzione ricorsiva ####
sceltaVar()

log_close()