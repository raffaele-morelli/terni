args <- commandArgs(trailingOnly = TRUE)

# if(!purrr::is_empty(args)) {
#   cat("argomenti => ", args, sep = "\n")
#   mese <- as.numeric(args[1])
# }else{
#   mese <- 2
# }


library(dplyr)
library(ggplot2)
library(ggspatial)
library(ggthemes)
library(glue)
library(purrr)
library(readr)
library(sf)
library(stringr)
library(terra)
library(viridis)

library(greekLetters)

rds_out_traccianti <- "rds_out_traccianti_test9"

dir.create(glue::glue("~/R/terni/tiff_out/{rds_out_traccianti}"), recursive = TRUE, showWarnings = FALSE)

res <- 100
grd <- 100

{
  dominio <- st_read("~/R/terni/data/dominio/dominio_100m.shp") # 109 col
  if(res == 100) { 
    n_col <- 109
  }else{
    n_col <- 54
  }
  
  terni_sez <- st_read("~/R/terni/data/shp/Terni_sez.shp") # sezioni di censimento
  
  pt_misura_utm32 <- st_read("~/R/terni/data/shp/punti_misura.shp")
  
  st_bbox(dominio) -> bbox
  r_extent <- c(as.numeric(bbox["xmin"]), as.numeric(bbox["xmax"]), as.numeric(bbox["ymin"]), as.numeric(bbox["ymax"]))
  
  fls <- list.files(glue("~/R/terni/{rds_out_traccianti}"), full.names = TRUE)
  
  selezione_terni <- read_csv("~/R/terni/selezione_terni.csv", col_names = FALSE, show_col_types = FALSE)
}


map(pull(selezione_terni), \(t) {
  glue("~/R/terni/{rds_out_traccianti}/{t}_200m_100res.RDS") 
}) %>% unlist() -> fls
# fls <- fls[1]
# f <- fls

purrr::walk(fls, \(f) {
  tools::file_path_sans_ext(f) %>% basename() -> fout
  writeLines(fout)
  
  elemento <- str_split(fout, pattern = "_")[[1]][1]
  
  titolo <- case_when(elemento == "PM10" ~ paste(elemento, "mg/m³"),
                      .default = paste(elemento, "ng/m³"))

  trcnt <- readRDS(f)
  trcnt_df <- do.call(rbind.data.frame, trcnt)

  walk(seq(1,12), \(mese) {
    trcnt_df1 <- trcnt_df[, mese]
    
    m <- matrix(trcnt_df1, ncol = n_col,  byrow = FALSE)  
    r <- raster::raster(m)
    
    mese <- str_pad(mese, 2, pad = "0")
    
    raster::extent(r) <- r_extent
    crs(r) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
    
    terra::writeRaster(r, glue::glue('~/R/terni/tiff_out/{rds_out_traccianti}/{fout}_{mese}.tif'), overwrite = TRUE)
  })
  
  # r_df <- as.data.frame(r, xy = TRUE) %>% 
  #   na.omit() %>% 
  #   setNames(c("x", "y", "value"))
  
  # readr::write_csv(r_df, file = glue::glue('~/R/terni/tiff_out/{rds_out_traccianti}/{fout}_{mese}.csv'))
  # hist(r_df$value)
  
  # ggplot(data = r_df) +
  #   geom_raster(aes(x = x, y = y, fill = value)) +
  #   geom_sf(data = st_crop(terni_sez, st_bbox(r)), color = "grey80", fill = "transparent", size = 0.5) +
  #   geom_sf(data = pt_misura_utm32, shape = 21, fill = "blue", color = "black", size = 3) +
  #   geom_sf_text(data = pt_misura_utm32, aes(label = Site), color = "blue", nudge_x = 125, nudge_y = 125 ) +
  #   # scale_fill_viridis_c(direction = -1, option = "magma") +
  #   scale_fill_distiller(palette = "Spectral", direction = -1, name = titolo) +
  #   coord_sf(datum = sf::st_crs(32632)) +
  #   annotation_scale(location = "br", width_hint = 0.25, pad_y = unit(0.75, "in"), pad_x = unit(1, "in")) +
  #   annotation_north_arrow(location = "br", which_north = "true", 
  #                          pad_x = unit(1.25, "in"), pad_y = unit(1, "in"),
  #                          style = north_arrow_fancy_orienteering) +
  #   theme_void() +
  #   theme(
  #     legend.position = "right", 
  #     # legend.title = element_blank(),
  #     plot.margin = unit(c(1,1,1,1), "cm")
  #     ) -> g
  # 
  # ggsave(filename = glue::glue('~/R/terni/png_out/{fout}_{mese}.png'), plot = g, bg = "white",
  #        width = 14, height = 9, units = "in", dpi = 150)
  
})



walk(pull(selezione_terni), \(t) {
  fs <- list.files("~/R/terni/tiff_out/rds_out_traccianti_test9", pattern = t, full.names = T)
  raster::stack(fs) %>% writeRaster(glue("~/R/terni/tiff_out/{t}.tif"))
}) 

map(pull(selezione_terni), \(t) {
  fs <- list.files("~/R/terni/tiff_out/rds_out_traccianti_test9", pattern = t, full.names = T)
  rs <- raster::stack(fs) 
  raster::cellStats(rs, stat = "mean") %>% as.vector()
}) -> listone

do.call(cbind, listone) %>% 
  as_tibble() %>% 
  setNames(pull(selezione_terni)) %>% 
  write_csv("~/R/terni/monthly_stats.csv")


# library(rasterVis)
# gplot(rs) + 
#   geom_tile(aes(fill = value)) +
#   facet_wrap(~ variable) +
#   # scale_fill_gradientn(colours = rev(terrain.colors(250))) +
#   # scale_fill_viridis(direction = -1) +
#   scale_fill_gradientn(colours = rev(magma(30))) +
#   coord_equal() +
#   theme_void()
