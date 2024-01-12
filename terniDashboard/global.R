{
  library(shiny)
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(summarytools)
  library(sf)
  library(glue)
  library(terra)
}


df <- readr::read_csv("/home/rmorelli/R/terni/data/dataframes/df_finale_raw_lod.csv", show_col_types = FALSE) 
models <- readRDS("/home/rmorelli/R/terni/rds_out/modelli_gaussian_clean.RDS")
# crssvld <- readRDS("/home/rmorelli/R/terni/rds_out/cross_validation_gaussian.RDS")

terni_sez <- st_read("/home/rmorelli/R/terni/data/shp/Terni_sez.shp") # sezioni di censimento
pt_misura_utm32 <- st_read("/home/rmorelli/R/terni/data/shp/punti_misura.shp")

dominio <- st_read(glue("/home/rmorelli/R/terni/data/dominio/dominio_100m.shp")) # 54 col
st_bbox(dominio) -> bbox

r_extent <- c(as.numeric(bbox["xmin"]), as.numeric(bbox["xmax"]), as.numeric(bbox["ymin"]), as.numeric(bbox["ymax"]))
rm(dominio)

pltnts <-  readRDS("/home/rmorelli/R/terni/rds_out/traccianti.RDS")

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs = c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}