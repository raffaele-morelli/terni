# There are three kinds of lies: lies, damned lies, and statistics.

## init ####
{
  args <- commandArgs(trailingOnly = TRUE)
  cat(args, sep = "\n")
  
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

  source('funzioni/f_buildMods.R')
  source('funzioni/f_bestMod.R')
  source('funzioni/f_sceltaVar.R')

  if(!purrr::is_empty(args)) {
    pltnt <- args[1] #### SET inquinante ####
    rds_dir <- args[2] ### SET directory ####
    df <- "raw"
  }else{
    pltnt <- "PM10"
    rds_dir <- "rds_gaussian_test4"
    df <- "raw"
  }

  blacklist_inquinanti <- read_csv("/home/rmorelli/R/terni/data/blacklist_inquinanti.csv", show_col_types = FALSE)
  
  if(pltnt %in% blacklist_inquinanti$pltnt) {
    stop("Sei nella blacklist!!!")
  }
  
  cat("############# ", pltnt, "\n")
  
  df <- read_csv(glue::glue("data/dataframes/df_finale_{df}.csv"), show_col_types = FALSE)

  index <- grep(pltnt, names(df))
  
  ## Variabili #####  
  names(df)[index] <- "value"

  # source("f_test.R") # !!!! warning !!!!! ####
  
  # v_variabili <- readRDS("~/R/terni/rds_out/v_variabili.RDS")
  
  # trick ####
  v_meteo_mean <- readRDS("~/R/terni/rds_out/v_meteo_mean.RDS")

  v_acciaieria <- c("cold_area", "hot_area", "scrapyard")
  
  # grep("200", v_spaziali) -> idx_spat
  # v_spaziali <- c(v_spaziali[idx_spat], "m_dis_ferr", v_acciaieria)
  v_spaziali_200 <- c('s8_sup_200', 's6_sup_200', 'imp_200', 'bh_200', 'pop_200', 'ml_200')
  v_spaziali_100 <- c('s8_sup_100', 's6_sup_100', 'imp_100', 'bh_100', 'pop_100', 'ml_100')
  
  v_variabili <- c(v_spaziali_100, v_spaziali_200, v_meteo_mean, v_acciaieria, "m_dis_ferr")
  
  v_variabili <- v_variabili[!(v_variabili %in% c("wdir_mean", "ptp_mean", "pwspeed_mean"))]
}


# Variabili "ambiente" ####
assign("v_variabili", v_variabili, envir = .GlobalEnv)
assign("AICS", list(), envir = .GlobalEnv)
assign("v_dead", c(), envir = .GlobalEnv)
assign("N", 0, envir = .GlobalEnv)
assign("pltnt", pltnt, envir = .GlobalEnv)
assign("kappas", readRDS("~/R/terni/rds_out/kappas.RDS"))
assign("rds_dir", rds_dir, envir = .GlobalEnv) # !!! directory di output !!! ####
assign("suffix", '', envir = .GlobalEnv) # !!! suffisso per i test !!! ####
assign("v_spaziali", c(v_spaziali_100, v_spaziali_200), envir = .GlobalEnv)

# assign("family", 'Gamma(link=identity)')
# assign("family", 'poisson(link=log)')
assign("family", 'gaussian(link=log)')

fn <- file.path(glue("log/{rds_dir}_{pltnt}.log"))
lf <- log_open(fn)

# ricorsione ####
sceltaVar()

log_close()

