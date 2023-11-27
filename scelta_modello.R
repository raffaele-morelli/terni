args <- commandArgs(trailingOnly = TRUE)
cat(args, sep = "\n")

pltnt <- args[1] #### SET inquinante ####

cat("############# ", pltnt, "\n")

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
  library(glue)
  
  setwd("~/R/terni")

  source('f_buildMods.R')
  source('f_bestMod.R')
  source('f_sceltaVar.R')
  
  df <- read_csv(glue::glue("data/dataframes/df_finale_lod.csv"), show_col_types = FALSE)
  names(df)
  index <- grep(pltnt, names(df))
  
  names(df)[index] <- "value"
  # findCappa <- function(var) {
  #   df[[var]] %>% unique() %>% length()
  # }
  # 
  # map(names(df), findCappa) -> cappas
  # names(cappas) <- names(df)  
}
# stop("ciaone")

## Variabili #####
{
  df_terni_mensili_correlazione <- read_excel("data/df_terni_mensili_correlazione.xlsx", sheet = " Variabili scelte")
  v_scelte <- df_terni_mensili_correlazione$`Variabili scelte`
  
  # idxs <- c(23)
  # v_scelte <- v_scelte[!v_scelte %in% v_scelte[idxs]]
  # idxs <- grep("pbl00_min|tp_median|pblmin_median", names(cappas))
  # cappas <- cappas[-idxs]
  
  v_meteo <- names(df)[93:182]
  
  v_meteo <- v_meteo[!(v_meteo %in% c("tp_min", "ptp_min", "ptp_median", "pbl00_min", "pblmin_min"))]
  v_scelte <- v_scelte[ !(v_scelte %in% c("tp_min", "ptp_min", "ptp_median", "pbl00_min", "pblmin_min")) ]
  
  v_meteo_mean <- grep("mean", names(df), value = TRUE) # le variabili meteo (media)
  v_buf200 <- grep("200", names(df), value = TRUE)[1:4] # solo i buffer 200
  v_urban_atlas <- grep("s8_sup_200|s7_sup_200|s6_sup_200|s5_sup_200|s4_sup_200|s3_sup_200|s2_sup_200|s1_sup_200", names(df), value = TRUE)
  v_acciaieria <- c("cold_area", "hot_area", "scrapyard")
  
  # v_variabili <- c("kndvi", v_meteo_mean, v_buf200, v_acciaieria, v_urban_atlas, "m_dis_ferr")
  v_variabili <- c("kndvi", v_scelte, v_buf200, v_acciaieria, v_urban_atlas, "m_dis_ferr")
}

# Variabili "ambiente" ####
assign("v_variabili", v_variabili, envir = .GlobalEnv)
assign("AICS", list(), envir = .GlobalEnv)
assign("v_dead", c(), envir = .GlobalEnv)
assign("N", 0, envir = .GlobalEnv)
assign("pltnt", pltnt, envir = .GlobalEnv)
assign("outdir", "rds/", envir = .GlobalEnv)

# assign("cappas", cappas, envir = .GlobalEnv)

fn <- file.path(glue("terni_{pltnt}.log"))
lf <- log_open(fn)

# funzione ricorsiva ####
sceltaVar()

log_close()

