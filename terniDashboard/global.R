{
  library(shiny)
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(summarytools)
  library(sf)
  library(glue)
  library(terra)
  
  # importante ####
  
  root <- "../"
  
  met <- "test9"
  rds_out_traccianti <- glue("rds_out_traccianti_{met}")
  
  run <- glue("rds_gaussian_{met}")
}
  
df <- readr::read_csv(glue("{root}/data/dataframes/df_finale_raw.csv"), show_col_types = FALSE)

models <- readRDS(glue("{root}/{run}/modelli_{met}_clean.RDS"))

terni_sez <- st_read(glue("{root}/data/shp/Terni_sez.shp")) # sezioni di censimento
pt_misura_utm32 <- st_read(glue("{root}/data/shp/punti_misura.shp"))
acciaieria <- st_read(glue("{root}/data/shp/acciaieria.shp"))

dominio <- st_read(glue("{root}/data/dominio/dominio_100m.shp")) # 54 col
st_bbox(dominio) -> bbox

r_extent <- c(as.numeric(bbox["xmin"]), as.numeric(bbox["xmax"]), as.numeric(bbox["ymin"]), as.numeric(bbox["ymax"]))
rm(dominio)

pltnts <- names(models)

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs = c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
