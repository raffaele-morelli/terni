args <- commandArgs(trailingOnly = TRUE)

rm(list = ls())

# if(!purrr::is_empty(args)) {
#   cat("argomenti => ", args, sep = "\n")
#   mese <- as.numeric(args[1])
# }else{
#   mese <- 2
# }

# init ####
{
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
  
  
  dominio <- st_read("~/R/terni/data/dominio/dominio_100m.shp") # 109 col
  if(res == 100) { 
    n_col <- 109
  }else{
    n_col <- 54
  }
  
  biomasse <- c("Cs_s", "K_s", "Rb_s", "Cd_s", "Pb_i")
  
  terni_sez <- st_read("~/R/terni/data/shp/Terni_sez.shp") # sezioni di censimento
  
  pt_misura_utm32 <- st_read("~/R/terni/data/shp/punti_misura.shp")
  acciaieria <- st_read("~/R/terni/data/acciaieria/acciaieria.shp")   # acciaieria
  
  dominio_redux <- st_read("~/R/terni/data/shp/dominio_redux_articolo.shp")
  
  st_bbox(dominio) -> bbox
  r_extent <- c(as.numeric(bbox["xmin"]), as.numeric(bbox["xmax"]), as.numeric(bbox["ymin"]), as.numeric(bbox["ymax"]))
  
  fls <- list.files(glue("~/R/terni/{rds_out_traccianti}"), full.names = TRUE)
  
  selezione_terni <- read_csv("~/R/terni/selezione_terni.csv", col_names = FALSE, show_col_types = FALSE)
  
  strade_utm32 <- st_read("~/R/terni/data/osm/strade_interesse.shp") # strade di interesse
  strade_utm32_filtered <- filter(strade_utm32, highway %in% c("trunk_link", "primary",  "tertiary",  "secondary", "secondary_link", "tertiary_link",  "trunk",  "primary_link"))
  
  
}


map(pull(selezione_terni), \(t) {
  glue("~/R/terni/{rds_out_traccianti}/{t}_200m_100res.RDS") 
}) %>% unlist() -> fls
# fls <- fls[2]
# f <- fls

# tiff per mese ####
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
    
    r_crop <- raster::crop(r, dominio_redux)
    
    terra::writeRaster(r_crop, glue::glue('~/R/terni/tiff_out/{rds_out_traccianti}/{fout}_{mese}.tif'), overwrite = TRUE)
    
  })
  
})


# tiff media annuale ####
walk(pull(selezione_terni), \(t) {
  fs <- list.files("~/R/terni/tiff_out/rds_out_traccianti_test9", pattern = t, full.names = T)
  rs <- raster::stack(fs)

  raster::calc(rs, fun = mean) %>% 
    writeRaster(glue("~/R/terni/tiff_out/{t}_mean.tif"), overwrite = TRUE)
  
})


# immagini articolo media annuale ####
walk(pull(selezione_terni), \(t) {
  fs <- list.files("~/R/terni/tiff_out/rds_out_traccianti_test9", pattern = t, full.names = T)
  rs <- raster::stack(fs)
  r <- raster::calc(rs, fun = mean)
  
  titolo <- case_when(t == "PM10" ~ paste(t, "mg/m³"),
                      .default = paste(t, "ng/m³"))
  
  r_df <- as.data.frame(r, xy = TRUE) %>%
    na.omit() %>%
    setNames(c("x", "y", "value"))

  ggplot(data = r_df) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    geom_sf(data = st_crop(strade_utm32, st_bbox(dominio_redux)), color = "grey80", fill = "transparent", size = 0.5) +
    # geom_sf(data = st_crop(terni_sez, st_bbox(r)), color = "grey80", fill = "transparent", size = 0.5) +
    geom_sf(data = pt_misura_utm32, shape = 21, fill = "dodgerblue", color = "black", size = 2) +
    geom_sf(data = filter(acciaieria, field_1 != "scrapyard"), shape = 8, fill = "dodgerblue4", color = "black", size = 2) +
    geom_sf_text(data = pt_misura_utm32, aes(label = Site),
                 color = "black", nudge_x = -80, nudge_y = -125,
                 size = 3) +
    # scale_fill_viridis_c(direction = -1, option = "magma") +
    scale_fill_distiller(palette = "Spectral", direction = -1, name = titolo) +
    coord_sf(datum = sf::st_crs(32632)) +
    annotation_scale(location = "br", width_hint = 0.25, pad_y = unit(0.75, "in"), pad_x = unit(1, "in")) +
    annotation_north_arrow(location = "br", which_north = "true",
                           pad_x = unit(1.25, "in"), pad_y = unit(1, "in"),
                           style = north_arrow_fancy_orienteering) +
    theme_void() +
    theme(
      legend.position = "right",
      plot.margin = unit(c(1,1,1,1), "cm")
    ) -> g

  ggsave(filename = glue::glue('~/R/terni/png_out/{t}_mean.png'), plot = g, bg = "white",
         width = 14, height = 9, units = "in", dpi = 72)
  
})


# immagini traccianti biomasse ####
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

walk(biomasse, \(b) {
  
  walk(stagioni, \(s) {
    fs <- list.files("~/R/terni/tiff_out/rds_out_traccianti_test9", 
                     pattern = glue("^{b}_(.*)_({s})"), 
                     full.names = T)
    # cat("", fs, sep = "\n")
    rs <- raster::stack(fs)
    r <- raster::calc(rs, fun = mean)
    
    stagione <- case_when(s == "01|02|03|11|12" ~ "Winter", 
                          s == "04|05|06" ~ "Spring", 
                          s == "07|08" ~"Summer", 
                          s == "09|10" ~ "Autumn")
    
    raster::calc(rs, fun = mean) %>% 
      writeRaster(glue("~/R/terni/tiff_out/biomassa/{b}_{stagione}.tif"), overwrite = TRUE)
  })
})


# immagini per stagione ####
walk(biomasse, \(b) {
  fs <- list.files("~/R/terni/tiff_out/biomassa",
                   pattern = glue("^{b}"), 
                   full.names = T)
  
  rs <- raster::stack(fs) 
  r_df <- as.data.frame(rs, xy = TRUE)
  
  colnames(r_df) <- str_remove(colnames(r_df), pattern = glue("{b}_"))
  
  titolo <- case_when(b == "PM10" ~ paste(b, "mg/m³"),
                      .default = paste(b, "ng/m³"))
  
  pivot_longer(r_df, cols = !c(x, y), names_to = "variable", values_to = "value") %>% 
    mutate(variable = factor(variable, levels = c("Spring", "Summer", "Autumn", "Winter"))) %>% 
    ggplot() +
    geom_raster(aes(x, y, fill = value)) +
    geom_sf(data = st_crop(strade_utm32, st_bbox(dominio_redux)), 
            color=alpha("grey",0.7), fill = "transparent", size = 0.01) +
    geom_sf(data = pt_misura_utm32, shape = 21, 
            fill = "dodgerblue", color = "black", size = 1) +
    scale_fill_distiller(palette = "Spectral", direction = -1, name = titolo) +
    facet_wrap(~variable) +
    theme_void() +
    theme(
      legend.position = "right",
      plot.margin = unit(c(1,1,1,1), "cm")
    ) +
    coord_sf() -> g
  
  ggsave(filename = glue::glue('~/R/terni/png_out/{b}_season.png'), plot = g, bg = "white",
         width = 14, height = 9, units = "in", dpi = 72)
})

# plot(rs)
  # na.omit() %>%
  # setNames(c("x", "y", "value"))
 
# library(rasterVis)
# library(tidyterra)
# 
# gplot(rs) +
#   geom_tile(aes(fill = value)) +
#   facet_wrap(~ variable) +
#   scale_fill_gradientn(colours = rev(terrain.colors(250))) +
#   # scale_fill_viridis(direction = -1) +
#   scale_fill_gradientn(colours = rev(magma(30))) +
#   coord_equal() 

