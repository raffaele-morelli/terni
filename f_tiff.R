library(sf)
library(glue)
library(stringr)
library(terra)
library(ggplot2)
library(dplyr)

pltnt <- "Cs_i"
for (i in seq(1:12)) {
  mese <- str_pad(i, 2, pad = "0")
  dir.create(glue::glue("~/R/terni/tiff_out/{mese}"), recursive = TRUE, showWarnings = FALSE)
}

mese <- 1
res <- 100
grd <- 100

{
  # dominio <- st_read(glue("~/R/terni/data/dominio/dominio_200m.shp")) # 54 col
  # dominio <- st_read("~/R/terni/data/dominio/dominio_200m_redux.shp") # 39 col
  
  dominio <- st_read("~/R/terni/data/dominio/dominio_100m.shp") # 109 col
  # dominio <- st_read("~/R/terni/data/dominio/dominio_100m_redux.shp") # 76 col
}

if(res == 100) { 
  n_col <- 109
}else{
  n_col <- 54
}

terni_sez <- st_read("~/R/terni/data/shp/Terni_sez.shp") # sezioni di censimento

pt_misura_utm32 <- st_read("~/R/terni/data/shp/punti_misura.shp")

st_bbox(dominio) -> bbox
r_extent <- c(as.numeric(bbox["xmin"]), as.numeric(bbox["xmax"]), as.numeric(bbox["ymin"]), as.numeric(bbox["ymax"]))

fls <- list.files("~/R/terni/rds_out_traccianti", pattern = glue("{grd}m_{res}res"), full.names = TRUE)


purrr::walk(fls, \(f) {
  tools::file_path_sans_ext(f) %>% basename() -> fout
  print(fout)

  trcnt <- readRDS(f)
  trcnt_df <- do.call(rbind.data.frame, trcnt)

  r <- matrix(trcnt_df[,mese], ncol = n_col,  byrow = FALSE) %>% raster::raster()
  
  mese <- str_pad(mese, 2, pad = "0")
  raster::extent(r) <- r_extent
  crs(r) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
  
  terra::writeRaster(r, glue::glue('~/R/terni/tiff_out/{mese}/{fout}_{mese}.tif'), overwrite = TRUE)
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
  # ggsave(filename = glue::glue('~/R/terni/png_out/{fout}_{mese}.png'), plot = g, 
  #        width = 1000, height = 600, units = "px", dpi = 72)
})


