# init ########
{
  library(dplyr)
  library(ggplot2)
  library(purrr)
  library(readr)
  library(stringr)
  library(terra)
  
  # questi sono gli indici per raggruppare le stagioni
  # 1 2016-11-01 I
  # 2 2016-12-01 I       
  # 3 2017-02-01 I       
  # 4 2017-03-01 P       
  # 5 2017-04-01 P       
  # 6 2017-05-01 P       
  # 7 2017-06-01 E       
  # 8 2017-08-01 E       
  # 9 2017-09-01 A       
  # 10 2017-11-01 A       
  # 11 2017-12-01 I       
  # 12 2018-01-01 I
  stagioni <- c("01|02|03|11|12", "04|05|06", "07|08", "09|10")
  
  traccianti_acciaieria <- read_csv("data/traccianti_acciaieria.csv", col_names = FALSE, show_col_types = FALSE) %>% pull() # traccianti acciaeria
  
  dominio_redux <- st_read("~/R/terni/data/shp/dominio_redux_articolo.shp")
  dominio <- st_read("~/R/terni/data/dominio/dominio_100m.shp") # 109 col
  
  strade_utm32 <- st_read("~/R/terni/data/osm/strade_interesse.shp") # strade di interesse
  pt_misura_utm32 <- st_read("~/R/terni/data/shp/punti_misura.shp") 
  acciaieria <- st_read("~/R/terni/data/acciaieria/acciaieria.shp")   # acciaieria
  
  pesi <- c(0.060,0.068,0.078,0.078,0.078,0.078,0.096,0.096,0.078,0.078,0.133,0.078)
}

saveImage <- function(r, pltnt) {
  r_df <- as.data.frame(r, xy = TRUE) %>%
    na.omit() %>%
    setNames(c("x", "y", "value"))
  
  titolo <- case_when(pltnt == "PM10" ~ paste(pltnt, "mg/m³"),
                      .default = paste(pltnt, "ng/m³"))
  
  ggplot(data = r_df) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    geom_sf(data = st_crop(strade_utm32, st_bbox(dominio_redux)), color = "grey80", fill = "transparent", size = 0.5) +
    geom_sf(data = pt_misura_utm32, shape = 21, fill = "dodgerblue", color = "black", size = 2) +
    geom_sf(data = filter(acciaieria, field_1 != "scrapyard"), shape = 8, fill = "dodgerblue4", color = "black", size = 2) +
    # geom_sf_text(data = pt_misura_utm32, aes(label = Site), color = "black", nudge_x = -80, nudge_y = -125, size = 3) +
    # scale_fill_viridis_c(direction = -1, option = "magma") +
    scale_fill_distiller(palette = "Spectral", direction = -1, name = titolo) +
    coord_sf(datum = sf::st_crs(32632)) +
    # annotation_scale(location = "br", width_hint = 0.25, pad_y = unit(0.75, "in"), pad_x = unit(1, "in")) +
    # annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(1.25, "in"), pad_y = unit(1, "in"), style = north_arrow_fancy_orienteering) +
    theme_void() +
    theme(legend.position = "right", plot.margin = unit(c(1,1,1,1), "cm")) -> g
  ggsave(filename = glue("~/R/terni/png_out/acciaieria/{pltnt}_mean.png"), g, 
         width = 14, height = 9,
         dpi = 600)
}


walk(traccianti_acciaieria, \(t) {
  
  pattern <- glue("^{t}_(.*)")
  fs <- list.files("~/R/terni/tiff_out/acciaieria", pattern = pattern, full.names = T)
  
  # cat("", pattern, fs, sep = "\n")
  rs <- rast(fs)
  
  rsw <- weighted.mean(rs, w = pesi)
  
  r <- mean(rsw)
  
  writeRaster(r, glue("~/R/terni/tiff_out/acciaieria/mean/{t}_mean.tif"), overwrite = TRUE)
  
  saveImage(r, t)
  
})

