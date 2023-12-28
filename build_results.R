# init ####
{
  library(dplyr)
  library(chron)
  library(readxl)
  library(glue)
  library(lubridate)
  library(readr)
  library(purrr)
  library(stringr)
  library(mgcv)
  library(logr)

  dir <- "gaussian"

  kappas <- readRDS("~/R/terni/rds_out/kappas.RDS")
} 

getSign <- function(mod) {
  summary(mod)$s.table %>% 
    as.data.frame() %>% 
    filter(`p-value` <= 0.05) %>% 
    rownames() -> v_sign
  return(v_sign)
}


getModel <- function(vars, df) {
  source("~/R/terni/funzioni/f_makeSpline.R")

  ms <- makeSpline(vars) %>% paste(collapse = " + ")
  
  ms <- paste("gam(value ~ ", ms, ", gamma=1.4, family=gaussian(link=log), data = df)")
  
  mod <- eval(parse(text = ms))
  return(mod)
}

pltnts <- list.files(glue("~/R/terni/rds_{dir}"), pattern = "^[A-Z]", full.names = TRUE) 

fn <- file.path(glue("log/clean_v_nsign.log"))
lf <- log_open(fn)

map(pltnts, \(pltnt) {
  inquinante <- tools::file_path_sans_ext(basename(pltnt))

  log_print(inquinante, hide_notes = TRUE)
  # df <- read_csv(glue::glue("data/dataframes/df_finale_lod_clean.csv"), show_col_types = FALSE)
  df <- read_csv("~/R/terni/data/dataframes/df_finale_raw.csv", show_col_types = FALSE)

  index <- grep(inquinante, names(df))
  names(df)[index] <- "value"
  
  rds <- readRDS(pltnt)
  mod <- getModel(names(rds), df)

}) -> models

names(models) <- tools::file_path_sans_ext(basename(pltnts))
saveRDS(models, file = glue("~/R/terni/rds_out/modelli_{dir}.RDS"))

map(names(models), \(m) {
  v_sign <- getSign(models[[m]])
  
  # df <- read_csv(glue::glue("data/dataframes/df_finale_lod_clean.csv"), show_col_types = FALSE)
  df <- read_csv("~/R/terni/data/dataframes/df_finale_raw.csv", show_col_types = FALSE)
  
  df %>% mutate(
    TOT_CR = Biomass_Burning_CR + Soil_Dust_CR + Steel_Plant_CR + Road_Dust_CR + Brake_Dust_CR,
    TOT_NCR = Biomass_Burning_NCR + Soil_Dust_NCR + Steel_Plant_NCR + Road_Dust_NCR + Brake_Dust_NCR
  ) -> df
  
  index <- grep(m, names(df))
  names(df)[index] <- "value"
  log_print(m, hide_notes = TRUE)

  
  lapply(v_sign, \(v) gsub("s\\(|\\)", '', v)) %>% unlist() -> v_sign
  
  # log_print( sprintf("After: %s", paste0(v_sign, collapse = " - " )), hide_notes = TRUE )
  mod <- getModel(v_sign, df)    
}) -> models_clean

names(models_clean) <- tools::file_path_sans_ext(basename(pltnts))

saveRDS(models_clean, file = glue("~/R/terni/rds_out/modelli_{dir}_clean.RDS"))


log_close()

