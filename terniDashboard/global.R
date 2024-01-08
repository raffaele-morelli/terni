library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(summarytools)
library(knitr)
library(kableExtra)
library(sf)
library(glue)
library(terra)


df <- readr::read_csv("/home/rmorelli/R/terni/data/dataframes/df_finale_raw.csv", show_col_types = FALSE)
models <- readRDS("/home/rmorelli/R/terni/rds_out/modelli_gaussian_clean.RDS")
# crssvld <- readRDS("/home/rmorelli/R/terni/rds_out/cross_validation_gaussian.RDS")

terni_sez <- st_read("/home/rmorelli/R/terni/data/shp/Terni_sez.shp") # sezioni di censimento
pt_misura_utm32 <- st_read("/home/rmorelli/R/terni/data/shp/punti_misura.shp")

dominio <- st_read(glue("/home/rmorelli/R/terni/data/dominio/dominio_200m.shp")) # 54 col
st_bbox(dominio) -> bbox

r_extent <- c(as.numeric(bbox["xmin"]), as.numeric(bbox["xmax"]), as.numeric(bbox["ymin"]), as.numeric(bbox["ymax"]))
rm(dominio)

pltnts <-  readRDS("/home/rmorelli/R/terni/rds_out/traccianti.RDS")

createPredictImage <- function(f, res) {
  
  if(res == 100) {
    n_col <- 109
  }else{
    n_col <- 54
  }
  # tools::file_path_sans_ext(f) %>% basename() -> fout
  trcnt <- readRDS(f)
  trcnt_df <- do.call(rbind.data.frame, trcnt)
  
  r <- matrix(trcnt_df[, 1], ncol = n_col,  byrow = FALSE) %>% raster::raster()
  
  raster::extent(r) <- r_extent
  crs(r) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
  r_df <- as.data.frame(r, xy = TRUE) %>% na.omit() %>% setNames(c("x", "y", "value"))
  
  # ggplot(data = r_df) +
  #   geom_raster(aes(x = x, y = y, fill = value)) +
  #   geom_sf(data = st_crop(terni_sez, st_bbox(r)), color = "grey90", fill = "transparent", size = 0.5) +
  #   geom_sf(data = pt_misura_utm32, shape = 21, fill = "lightgray", color = "black", size = 3) +
  #   scale_fill_viridis_c(direction = -1, option = "magma") +
  #   theme_void() +
  #   theme(legend.position = "none") + 
  #   coord_sf(datum = sf::st_crs(32632)) -> g
  # 
  # return(g)
  # ggsave(filename = glue::glue('~/R/terni/png_out/{fout}_{mese}.png'), plot = g, width = 1000, height = 600, units = "px", dpi = 72)
}

