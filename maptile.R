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
  library(greekLetters)
  library(maptiles)
  library(purrr)
  library(readr)
  library(sf)
  library(stringr)
  library(terra)
  library(tidyr)
  library(tidyterra)
  library(viridis)

  rds_out_traccianti <- "rds_out_traccianti_test9"
  
  # dir.create(glue::glue("~/R/terni/tiff_out/{rds_out_traccianti}"), recursive = TRUE, showWarnings = FALSE)
  
  res <- 100
  grd <- 100
  
  dominio <- st_read("~/R/terni/data/dominio/dominio_100m.shp") # 109 col
  n_col <- 109

  biomasse <- read_csv("/home/rmorelli/R/terni/data/biomasse.csv", show_col_types = FALSE, col_names = F) %>% pull()
  
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
  
}


pt_misura_utm32.mercator <- st_transform(pt_misura_utm32, 3857)
dominio.mercator <- st_transform(dominio_redux, 3857)
strade_utm32_filtered.mercator <- st_transform(strade_utm32_filtered, 3857)
  
tile <- get_tiles(dominio.mercator, 
                  # "Stadia.Stamen.Terrain",
                  "OpenStreetMap",
                  zoom = 13,
                  crop = TRUE,
                  apikey = "3c84548b-451e-4c70-9e79-e738ca6e00b7"
)

# Crop exactly to extent
tile_crop <- terra::crop(tile, dominio.mercator)
strade_utm32_filtered.mercator <- st_crop(strade_utm32_filtered.mercator, dominio.mercator)

pt_misura_utm32.mercator %>% 
  mutate(em_srcs = case_when(
    Site %in% c("RI", "MA") ~ "Power plant",
    Site %in% c("GI", "CR", "HG") ~ "Railway",
    Site %in% c("CZ", "HV", "SA", "UC", "CA", "CO") ~ "Busiest streets",
    Site %in% c("FA", "CB") ~ "Industrial biomass heating",
    Site %in% c("RI", "MA") ~ "Power plant",
    Site %in% c("FR", "BR", "AR", "PI", "PV", "LG") ~ "Domestic biomass heating",
    Site %in% c("RO", "OB", "PR", "CP") ~ "Steel plant"
  )) -> pt_misura_utm32.mercator

ggplot() +
  geom_spatraster_rgb(data = tile_crop, alpha = 0.5) +
  geom_sf(data = strade_utm32_filtered.mercator, alpha = 0.3) +
  geom_sf_text(data = pt_misura_utm32.mercator, aes(label = Site), 
               color = "black", fontface = "bold", nudge_x = -80, nudge_y = -155, size = 5) +
  geom_sf(data = pt_misura_utm32.mercator, aes(color = em_srcs), size = 3) +
  annotation_scale(location = "br", width_hint = 0.25, pad_y = unit(10, "mm"), pad_x = unit(20, "mm")) +
  annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(20, "mm"), pad_y = unit(14, "mm"), 
                         style = north_arrow_fancy_orienteering) +
  # scale_color_viridis_d()+
  coord_sf(datum = sf::st_crs(3857)) +
  theme_void() +
  theme(legend.position = "right") -> g
  ggsave(filename = glue("~/R/terni/tmp/mappabase.jpg"), g,
         width = 14, height = 9,
         dpi = 150)

