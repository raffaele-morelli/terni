# init ####

rm(list = ls())
args <- commandArgs(trailingOnly = TRUE)
cat(args, sep = "\n")

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
  library(forcats)

  if(!purrr::is_empty(args)) {
    rds_dir <- args[1] ### SET directory ####
  }else{
    rds_dir <- "test12"
  }
  
  source("ns_stagioni.R")
  # source("~/R/terni/funzioni/f_trick.R")
}

biomasse <- read_csv("/home/rmorelli/R/terni/data/biomasse.csv", show_col_types = FALSE, col_names = F) %>% pull()

blacklist_inquinanti <- read_csv("/home/rmorelli/R/terni/data/blacklist_inquinanti.csv", show_col_types = FALSE)

getSign <- function(mod) {
  summary(mod)$s.table %>% 
    as.data.frame() %>% 
    filter(`p-value` <= 0.05) %>% 
    rownames() -> v_sign_s
  
  summary(mod)$p.table %>% 
    as.data.frame() %>% 
    slice(-1) %>% 
    filter(`Pr(>|t|)` <= 0.05) %>% 
    rownames() -> v_sign_p
  
  # return(c(v_sign_s, v_sign_p))
  return(c(v_sign_s))
}

getModel <- function(vars, df, pltnt) {
  source("~/R/terni/funzioni/f_makeSpline.R")

  ms <- makeSpline(vars) %>% paste(collapse = " + ")
  
  if(pltnt %in% biomasse) {
    ms <- paste("gam(value ~ stagione + ", ms, ", gamma=1.4, family=gaussian(link=log), data = df)")
  }else{
    ms <- paste("gam(value ~ ", ms, ", gamma=1.4, family=gaussian(link=log), data = df)")
  }
  
  writeLines(ms)
  suppressWarnings(
    mod <- eval(parse(text = ms))
  )
  
  return(mod)
}

pltnts <- list.files(glue("~/R/terni/rds_gaussian_{rds_dir}"), pattern = "*.rds", full.names = TRUE) 

# fn <- file.path(glue("log/clean_v_nsign.log"))
# lf <- log_open(fn)

map(pltnts, \(f) {
  writeLines(f)
  pltnt <- tools::file_path_sans_ext(basename(f)) %>% str_remove(pattern = "_bio")
  
  # log_print(t, hide_notes = TRUE)
  df <- read_csv("~/R/terni/data/dataframes/df_finale_raw.csv", show_col_types = FALSE)
  df <- f_stagioni(df)

  index <- grep(pltnt, names(df))
  names(df)[index] <- "value"

  rds <- readRDS(f)
  mod <- getModel(names(rds), df, pltnt)

}) -> models

names(models) <- tools::file_path_sans_ext(basename(pltnts)) 
saveRDS(models, file = glue("~/R/terni/rds_gaussian_{rds_dir}/modelli_{rds_dir}.RDS"))

map(names(models), \(m) {
  writeLines(m)
  v_sign <- getSign(models[[m]])
  
  writeLines(v_sign)
  
  if(length(v_sign) == 0) {
    # log_print( sprintf("Nessuna variabile significativa per: %s", m ), hide_notes = TRUE )
    return()
  }
  
  df <- read_csv("~/R/terni/data/dataframes/df_finale_raw.csv", show_col_types = FALSE)
  df <- f_stagioni(df)
  
  index <- grep(m, names(df))
  names(df)[index] <- "value"
  
  # log_print(pltnt, hide_notes = TRUE)

  v_sign <- lapply(v_sign, \(v) gsub("s\\(|\\)", '', v)) %>% unlist()
  
  # log_print( sprintf("After: %s", paste0(v_sign, collapse = " - " )), hide_notes = TRUE )
  getModel(v_sign, df, m)
}) -> models_clean

names(models_clean) <- tools::file_path_sans_ext(basename(pltnts)) 

saveRDS(models_clean, file = glue("~/R/terni/rds_gaussian_{rds_dir}/modelli_{rds_dir}_clean.RDS"))

# log_close()

