args <- commandArgs(trailingOnly = TRUE)
cat(args, sep = "\n")

# There are three kinds of lies: lies, damned lies, and statistics.

pltnt <- args[1] #### SET inquinante ####
dir <- args[2] ### SET directory ####

# pltnt <- "Cr_i"
# dir <- "all"

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

  source('funzioni/f_buildMods.R')
  source('funzioni/f_bestMod.R')
  source('funzioni/f_sceltaVar.R')
  
  df <- read_csv(glue::glue("data/dataframes/df_finale_lod_clean.csv"), show_col_types = FALSE)

  index <- grep(pltnt, names(df))
  
  names(df)[index] <- "value"
}

## Variabili #####
{
  # v_variabili <- names(df)[84:172]
  # saveRDS(v_variabili, "v_variabili.RDS")
  v_variabili <- readRDS("~/R/terni/v_variabili.RDS")
}

# Variabili "ambiente" ####
assign("v_variabili", v_variabili, envir = .GlobalEnv)
assign("AICS", list(), envir = .GlobalEnv)
assign("v_dead", c(), envir = .GlobalEnv)
assign("N", 0, envir = .GlobalEnv)
assign("pltnt", pltnt, envir = .GlobalEnv)
assign("kappas", readRDS("kappas.RDS"))
assign("outdir", dir, envir = .GlobalEnv) # !!! directory di output !!! ####

fn <- file.path(glue("log/{outdir}/terni_{pltnt}.log"))
lf <- log_open(fn)

# ricorsione ####
sceltaVar()

log_close()

