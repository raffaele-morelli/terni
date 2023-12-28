library(sf)

pltnt <- "Cr_i"
for (i in seq(1:12)) {
  dir.create(glue::glue("~/R/terni/tiff_out/{i}"), recursive = TRUE, showWarnings = FALSE)
}

mese <- 1

{
  dominio <- st_read("~/R/terni/data/dominio/dominio_200m.shp") # 54 col
  # dominio <- st_read("~/R/terni/data/dominio/dominio_200m_redux.shp") # 39 col
  
  # dominio <- st_read("~/R/terni/data/dominio/dominio_100m.shp") # 109 col
  # dominio <- st_read("~/R/terni/data/dominio/dominio_100m_redux.shp") # 76 col
}

pt_misura_utm32 <- st_read("~/R/terni/data/shp/punti_misura.shp")
st_bbox(dominio) -> bbox

fls <- list.files("~/R/terni/rds_out_traccianti/", full.names = TRUE)

purrr::walk(fls, \(f) {
  tools::file_path_sans_ext(f) %>% basename() -> fout
  trcnt <- readRDS(f)
  trcnt_df <- do.call(rbind.data.frame, trcnt)
  
  r <- matrix(trcnt_df[,mese], ncol = 54,  byrow = FALSE) %>% raster::raster()
  
  mese <- str_pad(mese, 2, pad = "0")
  raster::extent(r) <- c(as.numeric(bbox["xmin"]), as.numeric(bbox["xmax"]),  as.numeric(bbox["ymin"]), as.numeric(bbox["ymax"]) )
  r <- terra::writeRaster(r, glue::glue('~/R/terni/tiff_out/{mese}/{fout}_{mese}.tif'), overwrite = TRUE)
})


