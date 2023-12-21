# init e dati ####
{
  library(sf)
  library(dplyr)
  library(ggplot2)
  # library(plotly)
  library(terra)
  # library(ncdf4) # package for netcdf manipulation
  # library(raster) # package for raster manipulation
  # library(rgdal) # package for geospatial analysis
  # library(chron)
  # library(readxl)
  # library(glue)
  # library(lubridate)
  # library(readr)
  # library(mapview)
  library(purrr)
  library(stringr)
  # library(readr)
  
  outdir <- "~/R/terni/data/dataframes"
} 

{
  terni_sez <- st_read("~/R/terni/data/shp/Terni_sez.shp") # sezioni di censimento
  terni_indicatori_sez <- readr::read_csv("~/R/terni/data/R10_indicatori_2021_sezioni_terni.csv", show_col_types = FALSE)
  
  terni_sez_pop <- inner_join(terni_sez, select(terni_indicatori_sez, SEZ2011, P1), 
                              by = join_by(SEZ2021 == SEZ2011))
  # terni_sez_pop %>%
  #   st_drop_geometry() %>%
  #   select(c(PRO_COM, SEZ, P1, SHAPE_Area)) -> terni_sez_pop
  
  acciaieria <- st_read("~/R/terni/data/acciaieria/acciaieria.shp")   # acciaieria
  dominio <- st_read("~/R/terni/data/dominio/dominio4_label.shp") %>% select(-Id) # dominio
  dominio <- tibble::rowid_to_column(dominio, var = "id")
  
  # punti di misura
  pt_misura_utm32 <- st_read("~/R/terni/data/shp/punti_misura.shp") 
  pt_misura_utm33 <- st_transform(pt_misura_utm32, 32633) # WGS84/UTM 32
  pt_misura_3035 <-  st_transform(pt_misura_utm32, 3035) # meglio trasformare il vettore piuttosto che il raster
  
  v_utm32 <- vect(pt_misura_utm32) # converto in SpatVector
  v_utm33 <- vect(pt_misura_utm33) # converto in SpatVector
  v_3035 <- vect(pt_misura_3035)
  
  # urban atlas 
  terni_ua_all <- st_read("~/R/terni/data/IT515L2_TERNI_UA2018_v013/Data/IT515L2_TERNI_UA2018_v013.gpkg")
  
  # prendiamo i codici di interesse
  codes_2018 <- c(11100, 11210, 11220, 11230, 11240, 12100, 12210, 12220, 12230)
  terni_ua_fltr <- filter(terni_ua_all, code_2018 %in% codes_2018)
  terni_ua_utm32 <- st_transform(terni_ua_fltr, 32632) # WGS84/UTM 32
  
  imperm <-  terra::rast("~/R/terni/data/tiff/rst_impermeabilizzazione_utm32.tif")
  imper_rst <- as.data.frame(imperm, xy = TRUE)
  
  bh <- rast("~/R/terni/data/bh/Dataset/IT515_TERNI_UA2012_DHM_V010.tif")
  
  dists <- c(25, 50, 75, 100, 200) # i buffer da considerare
  
  # creo le directory per gli output
  # dir.create("~/R/terni/out/building_heights", recursive = TRUE, showWarnings = FALSE)
  # dir.create("~/R/terni/out/imperviousness", recursive = TRUE, showWarnings = FALSE)
  # dir.create("~/R/terni/out/urban_atlas", recursive = TRUE, showWarnings = FALSE)
  # dir.create("~/R/terni/out/ndvi", recursive = TRUE, showWarnings = FALSE)
}


# codici urban atlas

# 11100: Continuous Urban fabric (S.L. > 80%)
# 11210: Discontinuous Dense Urban Fabric (S.L.: 50% - 80%)
# 11220: Discontinuous Medium Density Urban Fabric (S.L.: 30% - 50%)
# 11230: Discontinuous Low Density Urban Fabric (S.L.: 10% - 30%)
# 11240: Discontinuous very low density urban fabric (S.L. < 10%)
# 12100: Industrial, commercial, public, military and private units
# 12210: Fast transit roads and associated land
# 12220: Other roads and associated land
# 12230: Railways and associated land


getBufferUA <- function(dist, code, id) {
  pt_buffer <- st_buffer(dominio[id, "id"], dist, singleSide = FALSE, nQuadSegs = 17)

  var <- filter(terni_ua_utm32, code_2018 == code)

  st_intersection(var, pt_buffer) %>%
    mutate(area = st_area(geom)) %>%
    st_drop_geometry %>%
    summarise(area = sum(area)) %>% select(area) %>% as.numeric()
}
# getBufferUA(200, 12220, 2200) # test

# pt_buffer <- st_buffer(dominio[1820, "id"], 200, singleSide = FALSE, nQuadSegs = 17)
# g <- ggplot() + 
#   geom_sf(data = dominio, size = 0.1) + 
#   geom_sf(data = pt_buffer, fill = "green", color = "red") 
# g
# var <- filter(terni_ua_utm32, code_2018 == 12220)
# var_cropped <- st_crop(var, st_bbox(dominio))
# g + geom_sf(data = var_cropped, fill = "transparent")
#  
# inter <- st_intersection(var_cropped, pt_buffer)
# g <- g + geom_sf(data = var_cropped, fill = "transparent") +
#   geom_sf(data = inter, fill = "transparent", color = "orange")
# g

getBufferImperv <- function(dist, id) {
  b <- buffer(vect(dominio[id, "id"]), dist, quadsegs = 17)

  extract(imperm, b, xy = TRUE) %>%
    group_by(ID) %>%
    summarise(m = mean(rst_impermeabilizzazione_utm32)) %>% 
    select(m) %>% as.numeric()
}
# getBufferImperv(200, 1920) # test

# b <- buffer(vect( dominio[1920, "id"] ), 200, quadsegs = 17)
# inter <- extract(imperm, b, xy = TRUE) %>%
#   group_by(ID) %>%
#   summarise(m = mean(rst_impermeabilizzazione_utm32))
# 
# e <- extract(imperm, b, xy = TRUE)
# 
# ggplot() + geom_sf(data = dominio, size = 0.1) +
#   geom_sf(data = st_as_sf(b)) +
#   geom_raster(data = e, aes(x, y, fill = rst_impermeabilizzazione_utm32))


# # calcola l'altezza media per gli edifici che sono nel buffer
getBufferBH <- function(dist, id) {
  b <- buffer(vect(dominio[id, "id"]), dist, quadsegs = 17)

  extract(bh, b, xy = TRUE) %>%
    group_by(ID) %>%
    summarise(m = mean(IT515_TERNI_UA2012_DHM_V010, na.rm = TRUE), .groups = 'drop') %>% 
    select(m) %>% as.numeric()
}
# d <- st_transform(dominio[1920, "id"], 3035)
# getBufferBH(200, 1920) # se non lo trasformiamo noi lo fa in automatico

getBufferIntSEZ <- function(dist, id) {
  pt_buffer <- st_buffer(dominio[id, "id"], dist, singleSide = FALSE, nQuadSegs = 17)
  
  st_intersection(terni_sez_pop, pt_buffer) %>%
    mutate(area_int = as.numeric( st_area(geometry)) ) %>%
    mutate(PP1 = as.integer( (area_int / SHAPE_Area) * P1)) %>%
    select(SHAPE_Area, area_int, P1, PP1) %>%
    st_drop_geometry() %>%
    summarise(m = sum(PP1)) %>% select(m) %>% as.numeric()
}

# getBufferIntSEZ(200, 1920)


# # Strade OSM ####
# strade_utm32 <- st_read("data/osm/strade_interesse.shp")
# # somma dei metri lineari delle strade nel buffer
# 
# getBufferIntStrade <- function(dist) {
#   name <- str_pad(dist, 3, pad = "0") # importante per avere un ordine coerente
#   print( paste(dist, name, sep = "--"))
#   
#   pt_buffer <- st_buffer(pt_misura, dist, singleSide = FALSE, nQuadSegs = 17)
#   # nQuadSegs: con il tuning di questo par si può far corrispondere il buff con quello di arcGIS
#   st_intersection(strade_utm32, pt_buffer) %>% 
#     group_by(Site) %>% 
#     summarise(m = sum(st_length(geometry))) %>% st_drop_geometry() %>% 
#     write_csv(glue("~/R/terni/data/osm/df_strade_ml_{name}.csv"))
# }
# 
# walk(dists, ~ getBufferIntStrade(.x))
# 
# fls <- list.files(path = "~/R/terni/data/osm", pattern = glue("^df_strade_ml.*"), full.names = TRUE, recursive = TRUE)
# 
# lapply(fls, function(x) {
#   # df <- read_csv(x, col_types = cols(Site = col_skip()))
#   df <- read_csv(x, show_col_types = FALSE)
#   
#   right_join(
#     df, 
#     pt_misura %>% st_drop_geometry() %>% dplyr::select(Site), by = join_by(Site)
#   ) %>% 
#     arrange(Site) %>% dplyr::select(-Site)
#   
# }) -> dfs_osm
# 
# do.call(cbind, dfs_osm) %>%
#   cbind(sort(pt_misura$Site)) %>%
#   setNames(c(dists, "site")) %>%
#   write_csv(file = glue("{outdir}/df_strade_ml.csv"))
# 
# # distanza minima punto linea ####
# strade_utm32_filtered <- filter(strade_utm32, 
#                                 highway %in% c("trunk_link", "primary",  "tertiary",  "secondary", "secondary_link", "tertiary_link",  "trunk",  "primary_link"))
# 
# # plot(strade_utm32_filtered)
# getBufferStradeMinDist <- function(dist) {
#   name <- str_pad(dist, 3, pad = "0") # importante per avere un ordine coerente
#   print( paste(dist, name, sep = "--"))
#   
#   pt_buffer <- st_buffer(pt_misura, dist, singleSide = FALSE, nQuadSegs = 17)
#   # nQuadSegs: con il tuning di questo par si può far corrispondere il buff con quello di arcGIS
#   
#   # st_intersection(pt_buffer, strade_utm32_filtered) %>%
#   #   group_by(Site) %>% 
#   #   summarise(m = sum(st_length(geometry))) %>% 
#   #   arrange(Site) %>% 
#   #   st_cast() -> tmp_inters
#   
#   tmplist <- list()
#   for (s in pt_misura$Site) {
#     st_distance(
#       # filter(pt_misura, Site == s), filter(tmp_inters, Site == s)
#       filter(pt_misura, Site == s), strade_utm32_filtered
#     ) -> df
#     
#     tmplist[[s]] <- apply(df, 1, FUN = min)
#   }
#   
#   do.call(rbind, tmplist) %>% 
#     as.data.frame() %>%
#     setNames(dist) %>% 
#     write_csv(glue("~/R/terni/data/osm/df_strade_min_dist_{name}.csv"))
# }
# 
# walk(dists, ~ getBufferStradeMinDist(.x)) 
# fls <- list.files(path = "~/R/terni/data/osm", pattern = glue("^df_strade_min"), 
#                   full.names = TRUE, recursive = TRUE)
# 
# lapply(fls, function(x) {
#   df <- read_csv(x, show_col_types = FALSE)
# }) -> dfs_strade
# 
# do.call(cbind, dfs_strade) %>%
#   cbind(sort(pt_misura$Site)) %>%
#   setNames(c(dists, "site")) %>%
#   dplyr::select(c('200', "site")) %>% 
#   write_csv(file = glue("{outdir}/df_strade_min_dist.csv"))
# 
# # distanza minima punto ferrovia ####
# 
# ferrovia <- st_read("~/R/terni/data/osm/ferrovie.shp")
# 
# tmplist <- list()
# for (s in pt_misura$Site) {
#   st_distance(
#     filter(pt_misura, Site == s), ferrovia
#   ) -> df
#   
#   tmplist[[s]] <- apply(df, 1, FUN = min)
# }
# 
# do.call(rbind, tmplist) %>%
#   as.data.frame() %>%
#   cbind(names(tmplist)) %>% 
#   setNames(c("m_dis", "site")) %>% 
#   write_csv(file = glue("{outdir}/df_ferrovia_min_dist.csv"))
# 
# # distanza acciaieria ####
# tmplist <- list()
# for (s in pt_misura$Site) {
#   st_distance(
#     filter(pt_misura, Site == s), acciaieria
#   ) -> df
#   
#   tmplist[[s]] <- df
# }
# 
# 
# do.call(rbind, tmplist) %>%
#   as.data.frame() %>%
#   cbind(names(tmplist)) %>% 
#   setNames(c("cold_area", "hot_area", "scrapyard", "site")) %>% 
#   write_csv(file = glue("{outdir}/df_acc_dist.csv"))
# 
# 
# 
# # Ndvi ####
# 
# # nc_data <- nc_open("~/R/terni_asi/data/ndvi/T33TUH_201611_201801_S2_L3B_10m_NDVI_monthly_Terni.nc")
# pol_st <- stack("~/R/terni/data/ndvi/T33TUH_201611_201801_S2_L3B_10m_NDVI_monthly_Terni.nc")
# brick(pol_st) -> ndvi_rasterone
# # plot(pol_st)
# 
# getBufferRastNDVI <- function(dist, rst, var) {
#   name <- str_pad(dist, 3, pad = "0") # importante per avere un ordine coerente
#   print( paste(dist, name, sep = "--"))
#   
#   v1 <- buffer(v_utm33, dist, quadsegs = 17)
#   
#   extract(rst, v1, xy = TRUE) %>%
#     group_by(ID) %>%
#     mutate(ifelse(get(var) < 0, 0, get(var) )) %>% 
#     summarise(m = mean(get(var), na.rm = TRUE), .groups = 'drop') %>%
#     cbind(v1$Site) %>%
#     setNames(c("ID", "media", "site")) %>% 
#     write_csv(file = glue("~/R/terni/data/ndvi/out/rast_{var}_{name}.csv"))
# }
# 
# for (i in names(pol_st)) {
#   print(i)
#   outfile <- glue("~/R/terni/data/ndvi/{i}.tiff")
#   # writeRaster(ndvi_rasterone[[i]], outfile, format = 'GTiff', overwrite = T)
#   
#   rst <- rast(outfile)
#   walk(dists, ~ getBufferRastNDVI(.x, rst, i))
# }
# 
# # unisco i dataframe
# mesi <- names(ndvi_rasterone)
# 
# for(d in dists) {
#   name <- str_pad(d, 3, pad = "0")
#   print(name)
#   flsNDVI <- list.files(path = "~/R/terni/data/ndvi/out", pattern = glue("^rast.*_{name}\\.csv$"), full.names = TRUE )
#   
#   dfs <-  lapply(flsNDVI, function(x) { 
#     read_csv(x, col_types = cols(ID = col_skip(), site = col_skip()))
#   })
#   
#   do.call(cbind, dfs) %>%
#     setNames(mesi) %>% 
#     cbind(pt_misura$Site) %>% 
#     write_csv(file = glue("{outdir}/df_ndvi_{name}.csv"))
# }
# 
# 
# # Kndvi ####
# 
# # nc_data <- nc_open("~/R/terni_asi/data/kndvi/T33TUH_201611_201801_S2_L3B_10m_kNDVI_monthly_Terni.nc")
# pol_st <- stack("~/R/terni/data/kndvi/T33TUH_201611_201801_S2_L3B_10m_kNDVI_monthly_Terni.nc")
# brick(pol_st) -> kndvi_rasterone
# 
# # plot(pol_st)
# 
# getBufferRastKNDVI <- function(dist, rst, var) {
#   name <- str_pad(dist, 3, pad = "0") # importante per avere un ordine coerente
#   print( paste(dist, name, sep = "--"))
#   
#   v1 <- buffer(v_utm33, dist, quadsegs = 17)
#   
#   extract(rst, v1, xy = TRUE) %>%
#     group_by(ID) %>%
#     mutate(ifelse(get(var) < 0, 0, get(var) )) %>% 
#     summarise(m = mean(get(var), na.rm = TRUE), .groups = 'drop') %>%
#     cbind(v1$Site) %>%
#     setNames(c("ID", "media", "site")) %>% 
#     write_csv(file = glue("~/R/terni/data/kndvi/out/rast_{var}_{name}.csv"))
# }
# 
# for (i in names(pol_st)) {
#   print(i)
#   outfile <- glue("~/R/terni/data/kndvi/{i}.tiff")
#   writeRaster(kndvi_rasterone[[i]], outfile, format = 'GTiff', overwrite = T)
#   
#   rst <- rast(outfile)
#   
#   walk(dists, ~ getBufferRastKNDVI(.x, rst, i))
# }
# 
# # unisco i dataframe
# mesi <- names(kndvi_rasterone)
# 
# for(d in dists) {
#   name <- str_pad(d, 3, pad = "0")
#   print(name)
#   flsKNDVI <- list.files(path = "~/R/terni/data/kndvi/out", pattern = glue("^rast.*_{name}\\.csv$"), full.names = TRUE )
#   
#   dfs <-  lapply(flsKNDVI, function(x) { 
#     read_csv(x, col_types = cols(ID = col_skip(), site = col_skip()))
#   })
#   
#   do.call(cbind, dfs) %>%
#     setNames(mesi) %>%
#     cbind(pt_misura$Site) %>%
#     write_csv(file = glue("{outdir}/df_kndvi_{name}.csv"))
# }
# 
# 
# 
# # LAI ####
# 
# # nc_data <- nc_open("~/R/terni_asi/data/lai/T33TUH_201611_201801_S2_L3B_20m_LAI_monthly_Terni.nc")
# pol_st <- stack("~/R/terni/data/lai/T33TUH_201611_201801_S2_L3B_20m_LAI_monthly_Terni.nc")
# brick(pol_st) -> lai_rasterone
# 
# getBufferRastLAI <- function(dist, rst, var) {
#   name <- str_pad(dist, 3, pad = "0") # importante per avere un ordine coerente
#   print( paste(dist, name, sep = "--"))
#   
#   v1 <- buffer(v_utm33, dist, quadsegs = 17)
#   
#   extract(rst, v1, xy = TRUE) %>%
#     group_by(ID) %>%
#     mutate(ifelse(get(var) < 0, 0, get(var) )) %>% 
#     summarise(m = mean(get(var), na.rm = TRUE), .groups = 'drop') %>%
#     cbind(v1$Site) %>%
#     setNames(c("ID", "media", "site")) %>% 
#     write_csv(file = glue("~/R/terni/data/lai/out/rast_{var}_{name}.csv"))
# }
# 
# for (i in names(pol_st)) {
#   print(i)
#   outfile <- glue("~/R/terni/data/lai/{i}.tiff")
#   writeRaster(lai_rasterone[[i]], outfile, format = 'GTiff', overwrite = T)
#   
#   rst <- rast(outfile)
#   walk(dists, ~ getBufferRastLAI(.x, rst, i))
# }
# 
# # unisco i dataframe
# mesi <- names(lai_rasterone)
# 
# for(d in dists) {
#   name <- str_pad(d, 3, pad = "0")
#   print(name)
#   flsLAI <- list.files(path = "~/R/terni/data/lai/out", pattern = glue("^rast.*_{name}\\.csv$"), full.names = TRUE )
#   
#   dfs <-  lapply(flsLAI, function(x) { 
#     read_csv(x, col_types = cols(ID = col_skip(), site = col_skip()))
#   })
#   
#   do.call(cbind, dfs) %>%
#     setNames(mesi) %>% 
#     cbind(pt_misura$Site) %>% 
#     write_csv(file = glue("{outdir}/df_lai_{name}.csv"))
# }
# 
# 
# # plots ####
# library(ggrepel)
# library(ggthemes)
# cbind(pt_misura, st_coordinates(pt_misura)) -> pt_misura
# 
# g <- ggplot() + 
#   geom_sf(data = terni_sez, fill = "transparent") + 
#   geom_sf(data = dominio, color = "dodgerblue4", size = 0.4, alpha = 0.5) +
#   geom_sf(data = pt_misura, color = "black", size = 3) 
# g <- g + geom_label_repel(data = pt_misura, 
#                           aes(x = X, y = Y, label = Site), 
#                           min.segment.length = 0, max.overlaps = Inf)
# g +   geom_sf(data = acciaieria, color = "red", size = 3)  +
#   coord_sf(crs = 32632, xlim = c(793718.2, 803518.2), ylim = c(4712983, 4722783)) + theme_map()
# 
# ggsave("mappa.png")
# 