# init e dati ####
{
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(terra)
  library(purrr)
  library(stringr)

  outdir <- "~/R/terni/data/dataframes"

  terni_sez <- st_read("~/R/terni/data/shp/Terni_sez.shp") # sezioni di censimento
  terni_indicatori_sez <- readr::read_csv("~/R/terni/data/R10_indicatori_2021_sezioni_terni.csv", show_col_types = FALSE)
  
  terni_sez_pop <- inner_join(terni_sez, select(terni_indicatori_sez, SEZ2011, P1), 
                              by = join_by(SEZ2021 == SEZ2011))
  
  ferrovia <- st_read("~/R/terni/data/osm/ferrovie.shp")
  
  strade_utm32 <- st_read("~/R/terni/data/osm/strade_interesse.shp") # strade di interesse
  strade_utm32_filtered <- filter(strade_utm32, highway %in% c("trunk_link", "primary",  "tertiary",  "secondary", "secondary_link", "tertiary_link",  "trunk",  "primary_link"))

  acciaieria <- st_read("~/R/terni/data/acciaieria/acciaieria.shp")   # acciaieria
  acciaieria$field_1 <- c("cold_area", "hot_area", "scrapyard")
  
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
  codes_2018 <- c(11100, 11210, 11220, 11230, 11240, 12100, 12210, 12220)
  cod_str <- c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8")
  
  lista_ua <- as.list(codes_2018)
  names(lista_ua) <- paste0(cod_str, "_sup_200")
  
  terni_ua_fltr <- filter(terni_ua_all, code_2018 %in% codes_2018)
  terni_ua_utm32 <- st_transform(terni_ua_fltr, 32632) # WGS84/UTM 32
  
  imperm <-  terra::rast("~/R/terni/data/tiff/rst_impermeabilizzazione_utm32.tif")
  imper_rst <- as.data.frame(imperm, xy = TRUE)
  
  bh <- rast("~/R/terni/data/bh/Dataset/IT515_TERNI_UA2012_DHM_V010.tif")

  # pol_st <- raster::stack("~/R/terni/data/kndvi/T33TUH_201611_201801_S2_L3B_10m_kNDVI_monthly_Terni.nc")
  # raster::brick(pol_st) -> kndvi_rasterone
  kndvis <- list.files("~/R/terni/data/kndvi/", pattern = "*.tiff", full.names = TRUE)
  for (i in kndvis) {
    mese <- tools::file_path_sans_ext(basename(i))
    assign(paste(mese), terra::rast(i))
  }
  
  df_meteo <- readr::read_csv("~/R/terni/data/dataframes/df_terni_meteo_mensili_periodo.csv", show_col_types = FALSE) %>% arrange(data)
  # df_meteo <- cbind(df_meteo[,1:4], scale(df_meteo[,5:94]) )
  
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

cod_str <- c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8")

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
    select(m) %>% as.numeric() -> val
  
  if(!is.na(val)) {
    return(val)
  }else{
    return(0)
  }
}
# getBufferImperv(200, 1) # test

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
    select(m) %>% as.numeric() %>% round() -> val
  
  if(!is.na(val)) { return(val)}else{ return(0)}
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
    summarise(m = sum(PP1)) %>% select(m) %>% as.numeric() %>% round()
}
# getBufferIntSEZ(200, 1920)

getBufferIntStrade <- function(dist, id) {
  pt_buffer <- st_buffer(dominio[id, "id"], dist, singleSide = FALSE, nQuadSegs = 17)

  inter <- st_intersection(strade_utm32, pt_buffer) 
  if(nrow(inter) > 0 ) {
    inter %>% 
      summarise(m = sum(st_length(geometry))) %>% 
      st_drop_geometry() %>% 
      select(m) %>% as.numeric() %>% round()
  }else{
    return(0)
  }
}
# getBufferIntStrade(200, 1925)
 

getStradeMinDist <- function(dist, id) {
  return(
    as.numeric(min(st_distance(dominio[id, "id"], strade_utm32_filtered)) %>% round())
  )
}
# st_distance(dominio[1925, "id"], strade_utm32_filtered) -> df
# getStradeMinDist(200,1925)

getFerroMinDist <- function(dist, id) {
  return(
    as.numeric(min(st_distance(dominio[id, "id"], ferrovia)) %>% round())
  )
}

getAcciaMinDist <- function(dist, id, var) {
  return(
    as.numeric(min(st_distance(dominio[id, "id"], filter(acciaieria, field_1 == var) ) ) ) %>% round()
  )
}


getBufferRastKNDVI <- function(dist, rst, mese, id) {
  b <- buffer(vect(dominio[id, "id"]), dist, quadsegs = 17)
  
  extract(rst, b, xy = TRUE) %>%
    group_by(ID) %>%
    mutate(ifelse(get(mese) < 0, 0, get(mese) )) %>%
    summarise(m = mean(get(mese), na.rm = TRUE), .groups = 'drop') %>%
    setNames(c("ID", "media", "site")) %>% select(media) %>% as.numeric() 
}
i <- "X2016.11.02"
rst <- terra::rast(glue::glue("~/R/terni/data/kndvi/{i}.tiff"))
getBufferRastKNDVI(200, rst, "X2016.11.02", 1925)

# esempio cr_i ####
# df <- readr::read_csv("data/dataframes/df_finale_lod_clean.csv", show_col_types = FALSE)
df <- readr::read_csv("data/dataframes/df_finale_raw.csv", show_col_types = FALSE)
v_meteo <- names(df)[84:162]

v_ua <- c("s6_sup_200", "s8_sup_200")
v_acc <- c("cold_area", "hot_area", "scrapyard")
v_spa <- c("imp_200", "bh_200", "pop_200", "ml_200")

pltnt <- "Cr_i"
vars <- readRDS(glue::glue("~/R/terni/rds_gaussian/{pltnt}.rds")) %>% names()
modelli <- readRDS("~/R/terni/rds_out/modelli_all_clean.RDS")

index <- grep(pltnt, names(df), value = FALSE)
names(df)[index] <- "value"

gam_tdf <- mgcv::gam(formula(modelli[[pltnt]]), 
                     data = df, 
                     family = family(modelli[[pltnt]]))

modelli[[pltnt]]$model -> mod_data

library(logr)
library(lubridate)

new_ass <- seq(as.Date("2016-11-01"), by = "month", length.out = 15) %>% 
  as.data.frame() %>% 
  setNames(c("data"))


# test ####
{
  log_open(file_name = "domine.log")

  map(1500:1600, \(id) {
    # log_print(
    #   sprintf("s8: %s, s6: %s, cold_area: %s, hot_area: %s, scrapyard: %s, imp: %s,  bh: %s, pop: %s, mlstrade: %s, ferr: %s", 
    #           getBufferUA(200, lista_ua[["s8_sup_200"]], id),
    #           getBufferUA(200, lista_ua[["s6_sup_200"]], id),
    #           getAcciaMinDist(200, id, "cold_area"),
    #           getAcciaMinDist(200, id, "hot_area"),
    #           getAcciaMinDist(200, id, "scrapyard"),
    #           getBufferImperv(200, id), 
    #           getBufferBH(200, id), 
    #           getBufferIntSEZ(200, id),
    #           getBufferIntStrade(200, id),
    #           getFerroMinDist(200, id),
    #   hide_notes = TRUE))
    
    data.frame("variable" = c(getBufferUA(200, lista_ua[["s8_sup_200"]], id),
                              getBufferUA(200, lista_ua[["s6_sup_200"]], id),
                              getAcciaMinDist(200, id, "cold_area"),
                              getAcciaMinDist(200, id, "hot_area"),
                              getAcciaMinDist(200, id, "scrapyard"),
                              getBufferImperv(200, id), 
                              getBufferBH(200, id), 
                              getBufferIntSEZ(200, id),
                              getBufferIntStrade(200, id),
                              getFerroMinDist(200, id))) %>% t() -> df_spat
    # log_print(df_spat, hide_notes = TRUE)
    rownames(df_spat) <- NULL
    
    colnames(df_spat) <- c('s6_sup_200', 's8_sup_200', 'cold_area', 'hot_area', 'scrapyard', 'imp_200', 'bh_200', 'pop_200', 'ml_200', 'm_dis_ferr')

    # log_print(df_spat, hide_notes = TRUE)
    # log_print(t(df_spat), hide_notes = TRUE)
    map(df_meteo$data, \(d) {
      cbind(
        filter(df_meteo, data == d),
        df_spat
      ) -> pdf 
      
      # log_print(pdf, hide_notes = FALSE)
      saveRDS(pdf, glue::glue("~/R/terni/tmp/{d}.RDS"))
      mgcv::predict.gam(gam_tdf, newdata = pdf) -> mod
      log_print(mod)

    })
  }) -> ppipp
  
  log_close()
}

