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
} 

getSign <- function(mod) {
  summary(mod)$s.table %>% 
    as.data.frame() %>% 
    filter(`p-value` < 0.05) %>% 
    rownames() -> v_sign
  return(v_sign)
}


getModel <- function(vars, df) {
  ms <- 
    lapply(vars, function(x) {
      case_when(
        x == "tp_median" ~ paste0("s(", x, ", k=3)"),
        x == "u10m_min" ~ paste0("s(", x, ", k=5)"),
        x == "u10m_max" ~ paste0("s(", x, ", k=8)"),
        x == "v10m_median" ~ paste0("s(", x, ", k=9)"),
        x == "wspeed_min" ~ paste0("s(", x, ", k=8)"),
        x == "pblmin_median" ~ paste0("s(", x, ", k=3)"),
        x == "pbl00_median" ~ paste0("s(", x, ", k=6)"),
        x == "pblmin_IQR" ~ paste0("s(", x, ", k=9)"),
        x == "pbl00_min" ~ paste0("s(", x, ", k=1)"),
        x == "s7_sup_200" ~ paste0("s(", x, ", k=3)"),
        x == "s5_sup_200" ~ paste0("s(", x, ", k=9)"),
        x == "s1_sup_200" ~ paste0("s(", x, ", k=7)"),
        x == "u10m_median" ~ paste0("s(", x, ", k=9)"),
        x == "pwspeed_min" ~ paste0("s(", x, ", k=7)"),
        .default = paste0("s(", x, ")")
      )
    }) %>% paste(collapse = " + ") 
  
  ms <- paste("gam(value ~ ", ms, ", gamma=1.4, family=gaussian(link=log), data = df)")
  
  # cat("stringa modello: ", ms)
  mod <- eval(parse(text = ms))
  return(mod)
}

pltnts <- list.files(glue("~/R/terni/rds_{dir}"), pattern = "^[A-Z]", full.names = TRUE) 
df <- read_csv("~/R/terni/data/dataframes/df_finale_lod.csv", show_col_types = FALSE)

fn <- file.path(glue("log/clean_v_nsign.log"))
lf <- log_open(fn)

map(pltnts, \(pltnt) {
  inquinante <- tools::file_path_sans_ext(basename(pltnt))
  
  cat("\n## ", inquinante, "\n\n")
  
  index <- grep(inquinante, names(df))
  names(df)[index] <- "value"
  
  rds <- readRDS(pltnt)
  mod <- getModel(names(rds), df)
  
  v_sign <- getSign(mod) 
  
  log_print(inquinante)
  
  log_print( sprintf("Before: "), hide_notes = TRUE )
  log_print( summary(mod)$s.table %>% 
               as.data.frame(), hide_notes = TRUE)
  
  lapply(v_sign, \(v) gsub("s\\(|\\)", '', v)) %>% unlist() -> v_sign
  
  log_print( sprintf("After: %s", paste0(v_sign, collapse = " - " )), hide_notes = TRUE )
  
  
  mod <- getModel(v_sign, df)

}) -> models

names(models) <- tools::file_path_sans_ext(basename(pltnts))

saveRDS(models, file = glue("~/R/terni/rds_{dir}/modelli_{dir}_clean.RDS"))

log_close()

