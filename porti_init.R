library(sf)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(terra)

# urban atlas 2018
terni_all <- st_read("~/R/porti/data/IT515L2_TERNI_UA2018_v013/Data/IT515L2_TERNI_UA2018_v013.gpkg")
# com <- st_read("~/R/porti/Limiti01012021/Com01012021/Com01012021_WGS84.shp") %>% filter(PRO_COM == 55032) # limiti comunali

pt_misura <- st_read("punti_misura.shp")

# le variabil di iinteresse
terni_fltr <- filter(terni_all, code_2018 %in% c(11100, 11210, 11220, 11230, 11240, 12100, 12210, 12220))

terni_utm32 <- st_transform(terni_fltr, 32632) # WGS84/UTM 32

imperm <- rast("/home/rmorelli/R/porti/data/impermeabilizzazione_utm32.tif")
imper_rst <- as.data.frame(imperm, xy = TRUE)

# ggplot(data = imper_rst) + 
#   geom_raster(aes(x = x, y = y, fill = impermeabilizzazione_utm32)) +
#   scale_fill_viridis_c() + theme_void() + coord_fixed()

# 11100: Continuous Urban fabric (S.L. > 80%)
# 11210: Discontinuous Dense Urban Fabric (S.L.: 50% - 80%)
# 11220: Discontinuous Medium Density Urban Fabric (S.L.: 30% - 50%)
# 11230: Discontinuous Low Density Urban Fabric (S.L.: 10% - 30%)
# 11240: Discontinuous very low density urban fabric (S.L. < 10%)
# 12100: Industrial, commercial, public, military and private units
# 12210: Fast transit roads and associated land
# 12220: Other roads and associated land

codes_2018 <- c(11100, 11210, 11220, 11230, 11240, 12100, 12210, 12220)
dists <- c(025, 050, 075, 100, 200) # i buffer da considerare

getBufferInt <- function(dist, code) {
  name <- str_pad(dist, 3, pad = "0") # importante per avere un ordine coerente
  # print( paste(dist, code, name, sep = "--"))
  
  pt_buffer <- st_buffer(pt_misura, dist, singleSide = FALSE, nQuadSegs = 17)
  # nQuadSegs: con il tuning di questo par si può far corrispondere il buff con quello di arcGIS
  
  var <- filter(terni_utm32, code_2018 == code)

  st_intersection(var, pt_buffer) %>%
    mutate(area = st_area(geom)) %>%
    select(Site, area) %>%
    st_drop_geometry %>%
    group_by(Site) %>%   # TODO: verificare necessità
    summarise(area = sum(area)) %>%
    write_csv(file = glue::glue("out/area_{name}_{code}.csv"))
}

# i buffer per tutte le variabili
walk(codes_2018,  ~ walk(dists, ~ getBufferInt(.x, .y) , .y = .x))

# unisco i dataframe
map(codes_2018, function(c) {
  fls <- list.files(path = "out", pattern = glue::glue("^area_.*_{c}\\.csv$"), full.names = TRUE )
  
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
    write_csv(file = glue::glue("out/all_area_{c}.csv"))
})


# impermeabilizzazione ####

v <- vect(pt_misura) # converto in SpatVector

getBufferRast <- function(dist, code) {
  name <- str_pad(dist, 3, pad = "0") # importante per avere un ordine coerente
  # print( paste(dist, code, name, sep = "--"))
  
  v1 <- buffer(v, dist)
  
  extract(imperm, v1, xy = TRUE) %>%
    group_by(ID) %>%
    summarise(m = mean(impermeabilizzazione_utm32), .groups = 'drop') %>%
    cbind(v1$Site) %>% 
    write_csv(file = glue::glue("out/imp/rast_{name}_{code}.csv"))
}

# i impermeabilizzazione all'interno dei buffer
walk(codes_2018,  ~ walk(dists, ~ getBufferRast(.x, .y) , .y = .x))

# unisco i dataframe
map(codes_2018, function(c) {
  fls <- list.files(path = "out/imp", pattern = glue::glue("^rast_.*_{c}\\.csv$"), full.names = TRUE )
  
  dfs <-  lapply(fls, function(x) { read_csv(x, col_types = cols(ID = col_skip(), `v1$Site` = col_skip()))})
  
  do.call(cbind, dfs) %>%
    cbind(pt_misura$Site) %>%
    setNames(c(dists, "site")) %>%
    mutate(var = as.character(c)) %>% 
    write_csv(file = glue::glue("out/imp/all_imper_{c}.csv"))
})

# aggrego i due dataframe 
map(c("imper", "area"), function(v) {
  fls <- list.files(path = "out", pattern = glue::glue("^all_({v}).*\\.csv$"), full.names = TRUE, recursive = TRUE)
  lapply(fls, function(x) {
    read_csv(x)
  }) -> dfs
  
  do.call(rbind, dfs) %>% write_csv(file = glue::glue("/home/rmorelli/R/porti/data/df_{v}.csv"))
})


