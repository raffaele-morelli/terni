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

  dir <- "all"

  kappas <- readRDS("~/R/terni/kappas.RDS")
} 

getSign <- function(mod) {
  summary(mod)$s.table %>% 
    as.data.frame() %>% 
    filter(`p-value` <= 0.05) %>% 
    rownames() -> v_sign
  return(v_sign)
}


getModel <- function(vars, df) {
  source("f_makeSpline.R")
  # ms <-
  #   lapply(vars, function(x) {
  #     unlist(x) %>% unique() -> i
  # 
  #     k <- filter(kappas, var == i) %>% select(kappas) %>% as.numeric()
  # 
  #     # log_print(sprintf("var %s kappa %s", i, k))
  #     case_when(
  #       k > 4 ~  paste0("s(", x, ", k=5)"),
  #       k > 0 ~ paste0("s(", x, ", k=", k, ")"),
  #       .default = paste0("s(", x, ", k=", k, ")")  
  #     )
  #   }) %>% paste(collapse = " + ")
  ms <- makeSpline(vars) %>% paste(collapse = " + ")
  
  ms <- paste("gam(value ~ ", ms, ", gamma=1.4, family=gaussian(link=log), data = df)")
  
  mod <- eval(parse(text = ms))
  return(mod)
}

pltnts <- list.files(glue("~/R/terni/rds_{dir}"), pattern = "^[A-Z]", full.names = TRUE) 

map(names(df), \(var) {
  df[[var]] %>% unique() %>% length()
}) -> cappas
names(cappas) <- names(df)

fn <- file.path(glue("log/clean_v_nsign.log"))
lf <- log_open(fn)

map(pltnts, \(pltnt) {
  inquinante <- tools::file_path_sans_ext(basename(pltnt))
  
  log_print(inquinante)
  df <- read_csv("~/R/terni/data/dataframes/df_finale_lod.csv", show_col_types = FALSE)
  
  index <- grep(inquinante, names(df))
  names(df)[index] <- "value"
  
  rds <- readRDS(pltnt)
  mod <- getModel(names(rds), df)

}) -> models

names(models) <- tools::file_path_sans_ext(basename(pltnts))
saveRDS(models, file = glue("~/R/terni/rds_{dir}/modelli_{dir}.RDS"))

map(names(models), \(m) {
  v_sign <- getSign(models[[m]])
  
  df <- read_csv("~/R/terni/data/dataframes/df_finale_lod.csv", show_col_types = FALSE)
  
  index <- grep(m, names(df))
  names(df)[index] <- "value"
  log_print(m)
  
  # log_print( sprintf("Before: "), hide_notes = TRUE )
  # log_print( summary(mod)$s.table %>%
               # as.data.frame(), hide_notes = TRUE)
  
  lapply(v_sign, \(v) gsub("s\\(|\\)", '', v)) %>% unlist() -> v_sign
  
  # log_print( sprintf("After: %s", paste0(v_sign, collapse = " - " )), hide_notes = TRUE )
  mod <- getModel(v_sign, df)    
}) -> models_clean

names(models_clean) <- tools::file_path_sans_ext(basename(pltnts))

saveRDS(models_clean, file = glue("~/R/terni/rds_{dir}/modelli_{dir}_clean.RDS"))



log_close()

