# init e dati ####
{
  library(sf)
  library(dplyr)
  library(tidyverse)
  library(ggplot2)
  library(plotly)
  library(terra)
} 

{
  terni_all <- st_read("/home/rmorelli/R/terni/data/IT515L2_TERNI_UA2018_v013/Data/IT515L2_TERNI_UA2018_v013.gpkg")
  terni_sez <- st_read("/home/rmorelli/R/terni/data/shp/Terni_sez.shp")
  terni_indicatori_sez <- read_csv("data/R10_indicatori_2021_sezioni_terni.csv")
  
  # com <- st_read("/home/rmorelli/R/terni/Limiti01012021/Com01012021/Com01012021_WGS84.shp") %>% filter(PRO_COM == 55032) # limiti comunali
  
  pt_misura <- st_read("/home/rmorelli/R/terni/data/shp/punti_misura.shp")
  
  # variabili di interesse
  terni_fltr <- filter(terni_all, code_2018 %in% c(11100, 11210, 11220, 11230, 11240, 12100, 12210, 12220))
  
  terni_utm32 <- st_transform(terni_fltr, 32632) # WGS84/UTM 32
  
  imperm <- rast("/home/rmorelli/R/terni/data/tiff/rst_impermeabilizzazione_utm32.tif")
  imper_rst <- as.data.frame(imperm, xy = TRUE)
  
  dists <- c(025, 050, 075, 100, 200) # i buffer da considerare
  
  # variabili area/m² ####
  codes_2018 <- c(11100, 11210, 11220, 11230, 11240, 12100, 12210, 12220)
}

# creo le directory per gli output
dir.create("~/R/terni/out/building_heights", recursive = TRUE, showWarnings = FALSE)
dir.create("~/R/terni/out/imperviousness", recursive = TRUE, showWarnings = FALSE)
dir.create("~/R/terni/out/urban_atlas", recursive = TRUE, showWarnings = FALSE)

# 11100: Continuous Urban fabric (S.L. > 80%)
# 11210: Discontinuous Dense Urban Fabric (S.L.: 50% - 80%)
# 11220: Discontinuous Medium Density Urban Fabric (S.L.: 30% - 50%)
# 11230: Discontinuous Low Density Urban Fabric (S.L.: 10% - 30%)
# 11240: Discontinuous very low density urban fabric (S.L. < 10%)
# 12100: Industrial, commercial, public, military and private units
# 12210: Fast transit roads and associated land
# 12220: Other roads and associated land



getBufferInt <- function(dist, code) {
  name <- str_pad(dist, 3, pad = "0") # importante per avere un ordine coerente
  print( paste(dist, code, name, sep = "--"))
  
  pt_buffer <- st_buffer(pt_misura, dist, singleSide = FALSE, nQuadSegs = 17)
  # nQuadSegs: con il tuning di questo par si può far corrispondere il buff con quello di arcGIS
  
  var <- filter(terni_utm32, code_2018 == code)

  st_intersection(var, pt_buffer) %>%
    mutate(area = st_area(geom)) %>%
    select(Site, area) %>%
    st_drop_geometry %>%
    group_by(Site) %>%   # TODO: verificare necessità
    summarise(area = sum(area)) %>%
    write_csv(file = glue::glue("out/urban_atlas/area_{name}_{code}.csv"))
}

# i buffer per tutte le variabili
walk(codes_2018,  ~ walk(dists, ~ getBufferInt(.x, .y) , .y = .x))

# unisco i dataframe
map(codes_2018, function(c) {
  fls <- list.files(path = "out/urban_atlas", pattern = glue::glue("^area_.*_{c}\\.csv$"), full.names = TRUE )
  
  dfs <-  lapply(fls, 
                 function(x){ 
                   df <- read_csv(x) 
                   right_join(df, pt_misura %>% st_drop_geometry() %>% select(Site)) %>% 
                     select(area) 
                 })
  
  do.call(cbind, dfs) %>% 
    cbind(pt_misura$Site) %>% 
    setNames(c(dists, "site")) %>%
    mutate(var = as.character(c)) %>% 
    write_csv(file = glue::glue("out/urban_atlas/all_area_{c}.csv"))
})

fls <- list.files(path = "out/urban_atlas", pattern = glue::glue("^all_(area).*\\.csv$"), full.names = TRUE, recursive = TRUE)

lapply(fls, function(x) {
  read_csv(x)
}) -> dfs

do.call(rbind, dfs) %>%
  write_csv(file = glue::glue("/home/rmorelli/R/terni/data/df_urban_atlas.csv"))


# impermeabilizzazione ####

v <- vect(pt_misura) # converto in SpatVector

getBufferRast <- function(dist) {
  name <- str_pad(dist, 3, pad = "0") # importante per avere un ordine coerente
  print( paste(dist, name, sep = "--"))
  
  v1 <- buffer(v, dist, quadsegs = 17)
  
  extract(imperm, v1, xy = TRUE) %>%
    group_by(ID) %>%
    summarise(m = mean(rst_impermeabilizzazione_utm32), .groups = 'drop') %>%
    cbind(v1$Site) %>% 
    write_csv(file = glue::glue("out/imperviousness/rast_{name}.csv"))
}

# i impermeabilizzazione all'interno dei buffer
walk(dists, ~ getBufferRast(.x))

# unisco i dataframe

fls <- list.files(path = "out/imperviousness", pattern = glue::glue("^rast.*\\.csv$"), full.names = TRUE )
  
dfs <-  lapply(fls, function(x) { read_csv(x, col_types = cols(ID = col_skip(), `v1$Site` = col_skip()))})
  
do.call(cbind, dfs) %>%
  cbind(pt_misura$Site) %>%
  setNames(c(dists, "site")) %>%
  write_csv(file = glue::glue("/home/rmorelli/R/terni/data/df_imperviousness.csv"))
  

# building heights ####
# bh <- rast("/home/rmorelli/R/terni/data/tiff/rst_building_heights_utm32.tif")
bh <- rast("/home/rmorelli/R/terni/data/Dataset/IT515_TERNI_UA2012_DHM_V010.tif")
bh_utm32 <- project(bh, crs("epsg:32632"))

# calcola l'altezza media per gli edifici che sono nel buffer
# scrive un csv 
getBufferRast <- function(dist) {
  name <- str_pad(dist, 3, pad = "0") # importante per avere un ordine coerente
  print( paste(dist, name, sep = "--"))
  
  v1 <- buffer(v, dist, quadsegs = 17)
  
  extract(bh, v1, xy = TRUE) %>%
    group_by(ID) %>%
    summarise(m = mean(IT515_TERNI_UA2012_DHM_V010, na.rm = TRUE), .groups = 'drop') %>%
    cbind(v1$Site) %>% 
    write_csv(file = glue::glue("out/building_heights/rast_{name}.csv"))
}


# impermeabilizzazione all'interno dei buffer
walk(dists, ~ getBufferRast(.x))

fls <- list.files(path = "/home/rmorelli/R/terni/out/building_heights", pattern = glue::glue("^rast.*"), full.names = TRUE, recursive = TRUE)
lapply(fls, function(x) {
  read_csv(x, col_types = cols(ID = col_skip(), `v1$Site` = col_skip()))
}) -> dfs

do.call(cbind, dfs) %>%
  cbind(pt_misura$Site) %>%
  setNames(c(dists, "site")) %>% 
  write_csv(file = glue::glue("/home/rmorelli/R/terni/data/df_building_heights.csv"))

# sezioni di censimento ####
terni_sez_pop <- inner_join(terni_sez, select(terni_indicatori_sez, SEZ2011, P1), 
                            by = join_by(SEZ2021 == SEZ2011))

terni_sez_pop %>% 
  st_drop_geometry() %>% 
  select(c(PRO_COM, SEZ, P1, SHAPE_Area)) %>% 
  write_csv(file = glue::glue("/home/rmorelli/R/terni/data/df_popolazione.csv"))

getBufferInt <- function(dist) {
  name <- str_pad(dist, 3, pad = "0") # importante per avere un ordine coerente
  print( paste(dist, name, sep = "--"))
  
  pt_buffer <- st_buffer(pt_misura, dist, singleSide = FALSE, nQuadSegs = 17)
  # nQuadSegs: con il tuning di questo par si può far corrispondere il buff con quello di arcGIS
  
  st_intersection(terni_sez_pop, pt_buffer) %>% 
    mutate(area_int = as.numeric( st_area(geometry)) ) %>% 
    mutate(PP1 = as.integer( (area_int / SHAPE_Area) * P1)) %>% 
    select(Site, SHAPE_Area, area_int, P1, PP1) %>% 
    st_drop_geometry() %>% 
    group_by(Site) %>% 
    arrange(Site) %>% 
    summarise(m = sum(PP1)) %>% 
    setNames(c("site", dist)) %>%
    write_csv(file = glue::glue("out/popolazione/pop_{name}.csv"))
}

# i buffer per tutte le variabili
walk(dists, ~ getBufferInt(.x))


fls <- list.files(path = "/home/rmorelli/R/terni/out/popolazione", pattern = glue::glue("^pop.*"), full.names = TRUE, recursive = TRUE)

lapply(fls, function(x) {
  read_csv(x, col_types = cols(site = col_skip()))
  # read_csv(x)
}) -> dfs


do.call(cbind, dfs) %>%
  cbind(sort(pt_misura$Site)) %>%
  setNames(c(dists, "site")) %>%
  write_csv(file = glue::glue("/home/rmorelli/R/terni/data/df_popolazione_residente.csv"))

# meteo #####
library(readxl)
TERNI_PM_METEO <- read_excel("data/meteo/TERNI PM METEO.xlsx", 
                             col_types = c("date", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "text"))
names(TERNI_PM_METEO)[1] <- "date_time"

TERNI_PM_METEO %>% 
  mutate(
    anno = year(date_time), 
    mese = month(date_time), 
    giorno = day(date_time),
    ora = hour(date_time)
  ) %>% select(-note, -date_time) %>% 
  group_by(anno, mese, giorno, ora) %>%
  summarise(across(everything(), ~mean(.x, na.rm = TRUE))) %>% 
  mutate(data = make_date(anno,mese,giorno)) %>%  
  group_by(data) %>% select(-c(anno, mese, giorno, ora)) %>% 
  summarise(across(everything(), ~mean(.x, na.rm = TRUE))) -> terni_meteo_mensili

write_csv(terni_meteo_mensili, file = "data/df_terni_meteo_mensili.csv")

# OSM ####
strade <- st_read("~/R/terni/data/osm/strade_OSM.shp")
strade_utm32 <- st_transform(strade, 32632) # WGS84/UTM 32

