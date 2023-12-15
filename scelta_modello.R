args <- commandArgs(trailingOnly = TRUE)
cat(args, sep = "\n")

# There are three kinds of lies: lies, damned lies, and statistics.

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

  source('funzioni/f_buildMods.R')
  source('funzioni/f_bestMod.R')
  source('funzioni/f_sceltaVar.R')
  
  pltnt <- args[1] #### SET inquinante ####
  dir <- args[2] ### SET directory ####
  # pltnt <- "Li_s"
  # dir <- "gamma"
  
  cat("############# ", pltnt, "\n")
  
  df <- read_csv(glue::glue("data/dataframes/df_finale_lod_clean.csv"), show_col_types = FALSE)
  # df <- read_csv(glue::glue("data/dataframes/df_finale.csv"), show_col_types = FALSE)
  
  index <- grep(pltnt, names(df))
  
  ## Variabili #####  
  names(df)[index] <- "value"
  v_variabili <- readRDS("~/R/terni/rds_out/v_variabili.RDS")  
}


# Variabili "ambiente" ####
assign("v_variabili", v_variabili, envir = .GlobalEnv)
assign("AICS", list(), envir = .GlobalEnv)
assign("v_dead", c(), envir = .GlobalEnv)
assign("N", 0, envir = .GlobalEnv)
assign("pltnt", pltnt, envir = .GlobalEnv)
assign("kappas", readRDS("~/R/terni/rds_out/kappas.RDS"))
assign("outdir", dir, envir = .GlobalEnv) # !!! directory di output !!! ####

# assign("family", 'Gamma(link=identity)')
# assign("family", 'poisson(link=log)')
assign("family", 'gaussian(link=log)')

fn <- file.path(glue("log/{outdir}/{dir}_{pltnt}.log"))
lf <- log_open(fn)

# ricorsione ####
sceltaVar()

log_close()

