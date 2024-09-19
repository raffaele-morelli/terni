# init ####
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
    rds_dir <- "test9"
  }
  
  source("ns_stagioni.R")
} 
biomasse <- c("Cs_s", "K_s", "Rb_s", "Cd_s", "Pb_i")

df <- "df_finale_raw.csv" # dataframe

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
  
  suppressWarnings(
    mod <- eval(parse(text = ms))
  )
  
  return(mod)
}

pltnts <- list.files(glue("~/R/terni/rds_gaussian_{rds_dir}"), pattern = "*.rds", full.names = TRUE) 

fn <- file.path(glue("log/clean_v_nsign.log"))
lf <- log_open(fn)

map(pltnts, \(pltnt) {
  inquinante <- tools::file_path_sans_ext(basename(pltnt)) %>% str_remove(pattern = "_bio")

  log_print(inquinante, hide_notes = TRUE)
  df <- read_csv(glue("~/R/terni/data/dataframes/{df}"), show_col_types = FALSE)
  df <- f_stagioni(df)
  
  index <- grep(inquinante, names(df))
  names(df)[index] <- "value"

  rds <- readRDS(pltnt)
  mod <- getModel(names(rds), df, pltnt)

}) -> models

names(models) <- tools::file_path_sans_ext(basename(pltnts)) %>% str_remove(pattern = "_bio")
saveRDS(models, file = glue("~/R/terni/rds_gaussian_{rds_dir}/modelli_{rds_dir}.RDS"))

map(names(models), \(m) {
  v_sign <- getSign(models[[m]])
  
  if(length(v_sign) == 0) {
    log_print( sprintf("Nessuna variabile significativa per: %s", m ), hide_notes = TRUE )
    return()
  }
  df <- read_csv(glue("~/R/terni/data/dataframes/{df}"), show_col_types = FALSE)
  
  df <- f_stagioni(df)
  
  
  pltnt <- str_remove(m, pattern = "_bio")
  index <- grep(pltnt, names(df))
  names(df)[index] <- "value"
  
  log_print(pltnt, hide_notes = TRUE)

  lapply(v_sign, \(v) gsub("s\\(|\\)", '', v)) %>% unlist() -> v_sign
  
  # log_print( sprintf("After: %s", paste0(v_sign, collapse = " - " )), hide_notes = TRUE )
  mod <- getModel(v_sign, df, pltnt)    
}) -> models_clean

names(models_clean) <- tools::file_path_sans_ext(basename(pltnts)) %>% str_remove(pattern = "_bio")

saveRDS(models_clean, file = glue("~/R/terni/rds_gaussian_{rds_dir}/modelli_{rds_dir}_clean.RDS"))


log_close()

