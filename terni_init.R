# init e dati ####
{
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(terra)
  library(ncdf4) # package for netcdf manipulation
  library(raster) # package for raster manipulation
  library(rgdal) # package for geospatial analysis
  library(chron)
  library(readxl)
  library(glue)
  library(lubridate)
  library(readr)
  library(mapview)
  library(purrr)
  library(stringr)
  library(readr)
  
  outdir <- "~/R/terni/data/dataframes"
} 

{
  terni_sez <- st_read("~/R/terni/data/shp/Terni_sez.shp")
  terni_indicatori_sez <- read_csv("~/R/terni/data/R10_indicatori_2021_sezioni_terni.csv")
  
  # com <- st_read("~/R/terni_asi/Limiti01012021/Com01012021/Com01012021_WGS84.shp") %>% filter(PRO_COM == 55032) # limiti comunali
  
  pt_misura <- st_read("~/R/terni/data/shp/punti_misura.shp")
  v <- vect(pt_misura) # converto in SpatVector
  
  terni_all <- st_read("~/R/terni/data/IT515L2_TERNI_UA2018_v013/Data/IT515L2_TERNI_UA2018_v013.gpkg")
  
  # prendiamo i codici di interesse
  codes_2018 <- c(11100, 11210, 11220, 11230, 11240, 12100, 12210, 12220)
  
  terni_fltr <- filter(terni_all, code_2018 %in% codes_2018)
  terni_utm32 <- st_transform(terni_fltr, 32632) # WGS84/UTM 32
  
  imperm <-  terra::rast("~/R/terni/data/tiff/rst_impermeabilizzazione_utm32.tif")
  imper_rst <- as.data.frame(imperm, xy = TRUE)
  
  dists <- c(25, 50, 75, 100, 200) # i buffer da considerare
  
  # creo le directory per gli output
  dir.create("~/R/terni/out/building_heights", recursive = TRUE, showWarnings = FALSE)
  dir.create("~/R/terni/out/imperviousness", recursive = TRUE, showWarnings = FALSE)
  dir.create("~/R/terni/out/urban_atlas", recursive = TRUE, showWarnings = FALSE)
  dir.create("~/R/terni/out/ndvi", recursive = TRUE, showWarnings = FALSE)
}


# urban atlas ####

# 11100: Continuous Urban fabric (S.L. > 80%)
# 11210: Discontinuous Dense Urban Fabric (S.L.: 50% - 80%)
# 11220: Discontinuous Medium Density Urban Fabric (S.L.: 30% - 50%)
# 11230: Discontinuous Low Density Urban Fabric (S.L.: 10% - 30%)
# 11240: Discontinuous very low density urban fabric (S.L. < 10%)
# 12100: Industrial, commercial, public, military and private units
# 12210: Fast transit roads and associated land
# 12220: Other roads and associated land


getBufferUA <- function(dist, code) {
  name <- str_pad(dist, 3, pad = "0") # importante per avere un ordine coerente
  print( paste(dist, code, name, sep = "--"))
  
  pt_buffer <- st_buffer(pt_misura, dist, singleSide = FALSE, nQuadSegs = 17)
  # nQuadSegs: con il tuning di questo par si può far corrispondere il buff con quello di arcGIS
  
  var <- filter(terni_utm32, code_2018 == code)

  st_intersection(var, pt_buffer) %>% 
    mutate(area = st_area(geom)) %>% 
    st_drop_geometry %>%
    # select(Site, area) 
    group_by(Site) %>%   # TODO: verificare necessità
    summarise(area = sum(area)) %>%
    write_csv(file = glue("out/urban_atlas/area_{name}_{code}.csv"))
}

# i buffer per tutte le variabili
walk(codes_2018,  ~ walk(dists, ~ getBufferUA(.x, .y) , .y = .x))

# unisco i dataframe
map(codes_2018, function(c) {
  fls <- list.files(path = "out/urban_atlas", pattern = glue("^area_.*_{c}\\.csv$"), full.names = TRUE )
  
  dfs <-  lapply(fls, 
                 function(x) {
                   df <- read_csv(x, show_col_types = FALSE)
                   right_join(
                     df, 
                     pt_misura %>% st_drop_geometry() %>% dplyr::select(Site), by = join_by(Site)
                   ) %>% 
                     arrange(Site) %>% # se non ordino ottengo dataframe disallineate sulle righe
                     dplyr::select(area, Site) %>% setNames(c("area", "site"))
                 })
  
  do.call(cbind, dfs) -> tmp
  colnames(tmp)[c(1, 3, 5, 7, 9)] <- paste(dists, colnames(tmp)[c(1, 3, 5, 7, 9)], sep = "_")
  
  tmp %>% 
    dplyr::select(c(1, 3, 5, 7, 9, 10)) %>% 
    mutate(var = as.character(c)) %>% 
    write_csv(file = glue("out/urban_atlas/all_area_{c}.csv"))
})

fls <- list.files(path = "out/urban_atlas", pattern = glue("^all_(area).*\\.csv$"), full.names = TRUE, recursive = TRUE)

lapply(fls, function(x) {
  read_csv(x, show_col_types = FALSE)
}) -> dfs_ua

do.call(rbind, dfs_ua) %>% 
  write_csv(file = glue("{outdir}/df_urban_atlas.csv"))


# impermeabilizzazione ####

getBufferRastImp <- function(dist) {
  name <- str_pad(dist, 3, pad = "0") # importante per avere un ordine coerente
  print( paste(dist, name, sep = "--"))
  
  v1 <- buffer(v, dist, quadsegs = 17)
  
  extract(imperm, v1, xy = TRUE) %>%
    group_by(ID) %>%
    summarise(m = mean(rst_impermeabilizzazione_utm32), .groups = 'drop') %>%
    cbind(v1$Site) %>% 
    setNames(c("ID", "m", "Site")) %>%
    write_csv(file = glue("out/imperviousness/rast_{name}.csv"))
}

# i impermeabilizzazione all'interno dei buffer
walk(dists, ~ getBufferRastImp(.x))

# unisco i dataframe

fls <- list.files(path = "out/imperviousness", pattern = glue("^rast.*\\.csv$"), full.names = TRUE )
  
dfs_imp <-  lapply(fls, function(x) { 
  print(fls)
  read_csv(x, col_types = cols(ID = col_skip(), Site = col_skip() ) )
})
  
do.call(cbind, dfs_imp) %>%
  cbind(pt_misura$Site) %>%
  setNames(c(dists, "site")) %>%
  write_csv(file = glue("{outdir}/df_imperviousness.csv"))
  

# building heights ####
bh <- rast("~/R/terni/data/bh/Dataset/IT515_TERNI_UA2012_DHM_V010.tif")
# crs(bh, proj = TRUE)

pt_misura3035 <-  st_transform(pt_misura, 3035) # meglio trasformare il vettore piuttosto che il raster
v3035 <- vect(pt_misura3035)


# calcola l'altezza media per gli edifici che sono nel buffer
getBufferRastBH <- function(dist) {
  name <- str_pad(dist, 3, pad = "0") # importante per avere un ordine coerente
  print( paste(dist, name, sep = "--"))
  
  v1 <- buffer(v3035, dist, quadsegs = 17)
  
  extract(bh, v1, xy = TRUE) %>%
    group_by(ID) %>%
    summarise(m = mean(IT515_TERNI_UA2012_DHM_V010, na.rm = TRUE), .groups = 'drop') %>%
    cbind(v1$Site) %>% 
    write_csv(file = glue("out/building_heights/rast_{name}.csv"))
}

# building heights all'interno dei buffer
walk(dists, ~ getBufferRastBH(.x))

fls <- list.files(path = "~/R/terni/out/building_heights", pattern = glue("^rast.*"), full.names = TRUE, recursive = TRUE)
lapply(fls, function(x) {
  read_csv(x, col_types = cols(ID = col_skip(), `v1$Site` = col_skip()))
}) -> dfs_bh

do.call(cbind, dfs_bh) %>%
  cbind(pt_misura$Site) %>%
  setNames(c(dists, "site")) %>% 
  write_csv(file = glue("{outdir}/df_building_heights.csv"))

# sezioni di censimento ####
terni_sez_pop <- inner_join(terni_sez, dplyr::select(terni_indicatori_sez, SEZ2011, P1), by = join_by(SEZ2021 == SEZ2011))

terni_sez_pop %>% 
  st_drop_geometry() %>% 
  dplyr::select(c(PRO_COM, SEZ, P1, SHAPE_Area)) %>% 
  write_csv(file = glue("{outdir}/df_popolazione.csv"))

getBufferIntSEZ <- function(dist) {
  name <- str_pad(dist, 3, pad = "0") # importante per avere un ordine coerente
  print( paste(dist, name, sep = "--"))
  
  pt_buffer <- st_buffer(pt_misura, dist, singleSide = FALSE, nQuadSegs = 17)
  # nQuadSegs: con il tuning di questo par si può far corrispondere il buff con quello di arcGIS
  
  st_intersection(terni_sez_pop, pt_buffer) %>% 
    mutate(area_int = as.numeric( st_area(geometry)) ) %>% 
    mutate(PP1 = as.integer( (area_int / SHAPE_Area) * P1)) %>% 
    dplyr::select(Site, SHAPE_Area, area_int, P1, PP1) %>% 
    st_drop_geometry() %>% 
    group_by(Site) %>% 
    arrange(Site) %>% 
    summarise(m = sum(PP1)) %>% 
    setNames(c("site", dist)) %>%
    write_csv(file = glue("out/popolazione/pop_{name}.csv"))
}

# i buffer per tutte le variabili
walk(dists, ~ getBufferIntSEZ(.x))

fls <- list.files(path = "~/R/terni/out/popolazione", pattern = glue("^pop.*"), full.names = TRUE, recursive = TRUE)

lapply(fls, function(x) {
  read_csv(x, col_types = cols(site = col_skip()))
  # read_csv(x)
}) -> dfs_pop

do.call(cbind, dfs_pop) %>%
  cbind(sort(pt_misura$Site)) %>%
  setNames(c(dists, "site")) %>%
  write_csv(file = glue("{outdir}/df_popolazione_residente.csv"))

# meteo #####
# TERNI_PM_METEO <- read_excel("data/meteo/TERNI PM METEO.xlsx", 
#                              col_types = c("date", "numeric", "numeric", 
#                                            "numeric", "numeric", "numeric", 
#                                            "numeric", "numeric", "numeric", 
#                                            "numeric", "numeric", "text"))
# names(TERNI_PM_METEO)[1] <- "date_time"
# 
# TERNI_PM_METEO %>% 
#   mutate(
#     anno = year(date_time), 
#     mese = month(date_time), 
#     giorno = day(date_time),
#     ora = hour(date_time)
#   ) %>% dplyr::select(-note, -date_time) %>% 
#   group_by(anno, mese, giorno, ora) %>%
#   summarise(across(everything(), ~mean(.x, na.rm = TRUE)), .groups = "drop" ) %>% 
#   mutate(data = make_date(anno, mese)) %>%
#   # group_by(data) %>% 
#   dplyr::select(-c(anno, mese, giorno, ora)) %>% 
#   group_by(data) %>% 
#   summarise(across(everything(), ~mean(.x, na.rm = TRUE)), .groups = "drop") -> terni_meteo_mensili
# 
# write_csv(terni_meteo_mensili, file = glue("{outdir}/df_terni_meteo_mensili.csv"))

library(datiMeteo)
library(datiInquinanti)
stazioniAria %>% filter(comune == "Terni") %>% dplyr::select(station_eu_code) -> codici_staz

dati_meteo %>% 
  filter(station_eu_code == "IT1011A") %>% 
  filter(between(date, as.Date('2016-11-19'), as.Date('2018-02-19'))) %>% 
  dplyr::select(-station_eu_code, u10m, v10m) %>% 
    mutate(
      anno = year(date),
      mese = month(date),
      giorno = day(date)
    ) %>% group_by(anno, mese) %>%
  summarise(across(everything(), ~mean(.x, na.rm = TRUE)), .groups = "drop" ) %>%
  mutate(data = make_date(anno, mese)) %>% 
  dplyr::select(-c(anno, mese, giorno, station_code, coordx, coordy, altitude, altitudedem, date)) -> blocco_media

dati_meteo %>% 
  filter(station_eu_code == "IT1011A") %>% 
  filter(between(date, as.Date('2016-11-19'), as.Date('2018-02-19'))) %>% 
  dplyr::select(date, wspeed, pblmax, wspeed_max) %>%
  mutate(
    anno = year(date),
    mese = month(date),
    giorno = day(date),
    disp = wspeed*pblmax, 
    disp_max = wspeed*pblmax
  ) %>% group_by(anno, mese) %>%
  summarise(across(everything(), ~median(.x, na.rm = TRUE)), .groups = "drop" ) %>%
  mutate(data = make_date(anno, mese)) %>% 
  dplyr::select(-c(date, anno, mese, giorno, wspeed, pblmax, wspeed_max)) -> blocco_mediana

inner_join(blocco_media, blocco_mediana, by = "data") %>% 
  write_csv(file = glue("{outdir}/df_terni_meteo_mensili.csv"))

dati_meteo %>% 
  filter(station_eu_code == "IT1011A", between(date, as.Date('2016-11-19'), as.Date('2018-02-19'))) %>% 
  write_csv(file = glue("{outdir}/df_terni_meteo_giornalieri.csv"))

# modifica m.a ####
period_mese <- read_delim(
  "~/R/terni/data/period_mese.csv",
  delim = ";",
  escape_double = FALSE,
  col_types = cols(
    data_inizio = col_date(format = "%d/%m/%Y"),
    data_fine = col_date(format = "%d/%m/%Y")
  ),
  trim_ws = TRUE
) 

datiMeteo::dati_meteo %>% 
  filter(station_eu_code == "IT1011A", between(date, as.Date('2016-11-19'), as.Date('2018-02-19'))) %>% 
  full_join(period_mese, by = join_by( between(date, data_inizio, data_fine) ) ) %>% 
  filter(!is.na(month_y) ) %>% 
  dplyr::select(
    -c(
      data_inizio,
      data_fine,
      station_code,
      station_eu_code,
      coordx,
      coordy,
      altitude,
      altitudedem,
      date
    )
  )  %>% group_by(month_y, Season, data, associazione_ispra) %>%
  summarise(across(everything(),
                   list(
                     min = min,
                     max = max,
                     mean = mean,
                     median = median,
                     IQR = IQR
                   )
  ), .groups = "drop") %>% 
  write_csv(file = glue("{outdir}/df_terni_meteo_mensili_periodo.csv"))



# Strade OSM ####
strade_utm32 <- st_read("data/osm/strade_interesse.shp")
# somma dei metri lineari delle strade nel buffer

getBufferIntStrade <- function(dist) {
  name <- str_pad(dist, 3, pad = "0") # importante per avere un ordine coerente
  print( paste(dist, name, sep = "--"))
  
  pt_buffer <- st_buffer(pt_misura, dist, singleSide = FALSE, nQuadSegs = 17)
  # nQuadSegs: con il tuning di questo par si può far corrispondere il buff con quello di arcGIS
  st_intersection(strade_utm32, pt_buffer) %>% 
    group_by(Site) %>% 
    summarise(m = sum(st_length(geometry))) %>% st_drop_geometry() %>% 
    write_csv(glue("~/R/terni/data/osm/df_strade_ml_{name}.csv"))
}

walk(dists, ~ getBufferIntStrade(.x))

fls <- list.files(path = "~/R/terni/data/osm", pattern = glue("^df_strade_ml.*"), full.names = TRUE, recursive = TRUE)

lapply(fls, function(x) {
  # df <- read_csv(x, col_types = cols(Site = col_skip()))
  df <- read_csv(x, show_col_types = FALSE)
  
  right_join(
    df, 
    pt_misura %>% st_drop_geometry() %>% dplyr::select(Site), by = join_by(Site)
  ) %>% 
    arrange(Site) %>% dplyr::select(-Site)
  
}) -> dfs_osm

do.call(cbind, dfs_osm) %>%
  cbind(sort(pt_misura$Site)) %>%
  setNames(c(dists, "site")) %>%
  write_csv(file = glue("{outdir}/df_strade_ml.csv"))

# distanza minima punto linea ####
strade_utm32_filtered <- filter(strade_utm32, 
                                highway %in% c("trunk_link", "primary",  "tertiary",  "secondary", "secondary_link", "tertiary_link",  "trunk",  "primary_link"))

# plot(strade_utm32_filtered)
getBufferStradeMinDist <- function(dist) {
  name <- str_pad(dist, 3, pad = "0") # importante per avere un ordine coerente
  print( paste(dist, name, sep = "--"))
  
  pt_buffer <- st_buffer(pt_misura, dist, singleSide = FALSE, nQuadSegs = 17)
  # nQuadSegs: con il tuning di questo par si può far corrispondere il buff con quello di arcGIS
  
  # st_intersection(pt_buffer, strade_utm32_filtered) %>%
  #   group_by(Site) %>% 
  #   summarise(m = sum(st_length(geometry))) %>% 
  #   arrange(Site) %>% 
  #   st_cast() -> tmp_inters
  
  tmplist <- list()
  for (s in pt_misura$Site) {
    st_distance(
      # filter(pt_misura, Site == s), filter(tmp_inters, Site == s)
      filter(pt_misura, Site == s), strade_utm32_filtered
      ) -> df

    tmplist[[s]] <- apply(df, 1, FUN = min)
  }
  
  do.call(rbind, tmplist) %>% 
    as.data.frame() %>%
    setNames(dist) %>% 
  write_csv(glue("~/R/terni/data/osm/df_strade_min_dist_{name}.csv"))
}

walk(dists, ~ getBufferStradeMinDist(.x)) 
fls <- list.files(path = "~/R/terni/data/osm", pattern = glue("^df_strade_mim"), 
                  full.names = TRUE, recursive = TRUE)

lapply(fls, function(x) {
  df <- read_csv(x)
}) -> dfs_strade

do.call(cbind, dfs_strade) %>%
  cbind(sort(pt_misura$Site)) %>%
  setNames(c(dists, "site")) %>%
  dplyr::select(c('200', "site")) %>% 
  write_csv(file = glue("{outdir}/df_strade_min_dist.csv"))

# acciaieria ####
acciaieria <- st_read("~/R/terni/data/acciaieria/acciaieria.shp")

tmplist <- list()
for (s in pt_misura$Site) {
  st_distance(
    filter(pt_misura, Site == s), acciaieria
  ) -> df
  
  tmplist[[s]] <- df
}

do.call(rbind, tmplist) %>%
  as.data.frame() %>%
  cbind(names(tmplist)) %>% 
  setNames(c("cold area", "hot area", "scrapyard", "site")) %>% 
  write_csv(file = glue("{outdir}/df_acc_dist.csv"))

# punti utm33 ####
pt_misura_utm33 <- st_transform(pt_misura, 32633) # WGS84/UTM 32
v_utm33 <- vect(pt_misura_utm33) # converto in SpatVector

# Ndvi ####

# nc_data <- nc_open("~/R/terni_asi/data/ndvi/T33TUH_201611_201801_S2_L3B_10m_NDVI_monthly_Terni.nc")
pol_st <- stack("~/R/terni/data/ndvi/T33TUH_201611_201801_S2_L3B_10m_NDVI_monthly_Terni.nc")
brick(pol_st) -> ndvi_rasterone
# plot(pol_st)

getBufferRastNDVI <- function(dist, rst, var) {
  name <- str_pad(dist, 3, pad = "0") # importante per avere un ordine coerente
  print( paste(dist, name, sep = "--"))
  
  v1 <- buffer(v_utm33, dist, quadsegs = 17)
  
  extract(rst, v1, xy = TRUE) %>%
    group_by(ID) %>%
    mutate(ifelse(get(var) < 0, 0, get(var) )) %>% 
    summarise(m = mean(get(var), na.rm = TRUE), .groups = 'drop') %>%
    cbind(v1$Site) %>%
    setNames(c("ID", "media", "site")) %>% 
    write_csv(file = glue("~/R/terni/data/ndvi/out/rast_{var}_{name}.csv"))
}

for (i in names(pol_st)) {
  print(i)
  outfile <- glue("~/R/terni/data/ndvi/{i}.tiff")
  # writeRaster(ndvi_rasterone[[i]], outfile, format = 'GTiff', overwrite = T)
  
  rst <- rast(outfile)
  walk(dists, ~ getBufferRastNDVI(.x, rst, i))
}

# unisco i dataframe
mesi <- names(ndvi_rasterone)

for(d in dists) {
  name <- str_pad(d, 3, pad = "0")
  print(name)
  flsNDVI <- list.files(path = "~/R/terni/data/ndvi/out", pattern = glue("^rast.*_{name}\\.csv$"), full.names = TRUE )
  
  dfs <-  lapply(flsNDVI, function(x) { 
    read_csv(x, col_types = cols(ID = col_skip(), site = col_skip()))
  })
  
  do.call(cbind, dfs) %>%
    setNames(mesi) %>% 
    cbind(pt_misura$Site) %>% 
    write_csv(file = glue("{outdir}/df_ndvi_{name}.csv"))
}


# Kndvi ####

# nc_data <- nc_open("~/R/terni_asi/data/kndvi/T33TUH_201611_201801_S2_L3B_10m_kNDVI_monthly_Terni.nc")
pol_st <- stack("~/R/terni/data/kndvi/T33TUH_201611_201801_S2_L3B_10m_kNDVI_monthly_Terni.nc")
brick(pol_st) -> kndvi_rasterone

# plot(pol_st)

getBufferRastKNDVI <- function(dist, rst, var) {
  name <- str_pad(dist, 3, pad = "0") # importante per avere un ordine coerente
  print( paste(dist, name, sep = "--"))
  
  v1 <- buffer(v_utm33, dist, quadsegs = 17)
  
  extract(rst, v1, xy = TRUE) %>%
    group_by(ID) %>%
    mutate(ifelse(get(var) < 0, 0, get(var) )) %>% 
    summarise(m = mean(get(var), na.rm = TRUE), .groups = 'drop') %>%
    cbind(v1$Site) %>%
    setNames(c("ID", "media", "site")) %>% 
    write_csv(file = glue("~/R/terni/data/kndvi/out/rast_{var}_{name}.csv"))
}

for (i in names(pol_st)) {
  print(i)
  outfile <- glue("~/R/terni/data/kndvi/{i}.tiff")
  # writeRaster(kndvi_rasterone[[i]], outfile, format = 'GTiff', overwrite = T)
  
  rst <- rast(outfile)
  
  walk(dists, ~ getBufferRastKNDVI(.x, rst, i))
}

# unisco i dataframe
mesi <- names(kndvi_rasterone)

for(d in dists) {
  name <- str_pad(d, 3, pad = "0")
  print(name)
  flsKNDVI <- list.files(path = "~/R/terni/data/kndvi/out", pattern = glue("^rast.*_{name}\\.csv$"), full.names = TRUE )
  
  dfs <-  lapply(flsKNDVI, function(x) { 
    read_csv(x, col_types = cols(ID = col_skip(), site = col_skip()))
  })
  
  do.call(cbind, dfs) %>%
    setNames(mesi) %>%
    cbind(pt_misura$Site) %>%
    write_csv(file = glue("{outdir}/df_kndvi_{name}.csv"))
}



# LAI ####

# nc_data <- nc_open("~/R/terni_asi/data/lai/T33TUH_201611_201801_S2_L3B_20m_LAI_monthly_Terni.nc")
pol_st <- stack("~/R/terni/data/lai/T33TUH_201611_201801_S2_L3B_20m_LAI_monthly_Terni.nc")
brick(pol_st) -> lai_rasterone

getBufferRastLAI <- function(dist, rst, var) {
  name <- str_pad(dist, 3, pad = "0") # importante per avere un ordine coerente
  print( paste(dist, name, sep = "--"))
  
  v1 <- buffer(v_utm33, dist, quadsegs = 17)
  
  extract(rst, v1, xy = TRUE) %>%
    group_by(ID) %>%
    mutate(ifelse(get(var) < 0, 0, get(var) )) %>% 
    summarise(m = mean(get(var), na.rm = TRUE), .groups = 'drop') %>%
    cbind(v1$Site) %>%
    setNames(c("ID", "media", "site")) %>% 
    write_csv(file = glue("~/R/terni/data/lai/out/rast_{var}_{name}.csv"))
}

for (i in names(pol_st)) {
  print(i)
  outfile <- glue("~/R/terni/data/lai/{i}.tiff")
  writeRaster(lai_rasterone[[i]], outfile, format = 'GTiff', overwrite = T)
  
  rst <- rast(outfile)
  walk(dists, ~ getBufferRastLAI(.x, rst, i))
}

# unisco i dataframe
mesi <- names(lai_rasterone)

for(d in dists) {
  name <- str_pad(d, 3, pad = "0")
  print(name)
  flsLAI <- list.files(path = "~/R/terni/data/lai/out", pattern = glue("^rast.*_{name}\\.csv$"), full.names = TRUE )
  
  dfs <-  lapply(flsLAI, function(x) { 
    read_csv(x, col_types = cols(ID = col_skip(), site = col_skip()))
  })
  
  do.call(cbind, dfs) %>%
    setNames(mesi) %>% 
    cbind(pt_misura$Site) %>% 
    write_csv(file = glue("{outdir}/df_lai_{name}.csv"))
}


# plots ####
g <- ggplot(terni_sez) + geom_sf() 
g + geom_sf_label(data = pt_misura, aes(label = Site)) 
g + geom_sf(data = acciaieria, color = "red") + coord_sf(crs = 4326, xlim = c(12.58, 12.70), ylim = c(42.54, 42.59))
