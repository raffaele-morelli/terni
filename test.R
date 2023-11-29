# init e dati ####
{
  # library(sf)
  library(dplyr)
  library(ggplot2)
  library(chron)
  library(readxl)
  library(glue)
  library(lubridate)
  library(readr)
  library(purrr)
  library(stringr)
  # library(mgcv)
  # library(gratia)

} 

limiti <- read_excel("data/limiti.xlsx")

for (i in names(limiti)) {
  
  if(file.exists(glue("~/R/terni/rds_mean/{i}.rds")) ) {
    # cat(i, "\n")
  }else{
    cat("Rscript scelta_modello.R", i, "mean &\n")
  }
}