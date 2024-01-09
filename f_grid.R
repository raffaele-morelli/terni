# init e dati ####
{
  args <- commandArgs(trailingOnly = TRUE)
  cat(args, sep = "\n")

    # SET tracciante ####
  pltnt <- "Cs_i"
  # pltnt <- args[1]
  dist <- 100
  res <- 100
  
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(terra)
  library(purrr)
  library(stringr)
  library(logr)
  library(lubridate)
  library(glue)

  outdir <- "~/R/terni/data/dataframes"

  terni_sez <- st_read("~/R/terni/data/shp/Terni_sez.shp") # sezioni di censimento
  terni_indicatori_sez <- readr::read_csv("~/R/terni/data/R10_indicatori_2021_sezioni_terni.csv", show_col_types = FALSE)
  
  terni_sez_pop <- inner_join(terni_sez, select(terni_indicatori_sez, SEZ2011, P1), by = join_by(SEZ2021 == SEZ2011))
  
  ferrovia <- st_read("~/R/terni/data/osm/ferrovie.shp")
  
  strade_utm32 <- st_read("~/R/terni/data/osm/strade_interesse.shp") # strade di interesse
  strade_utm32_filtered <- filter(strade_utm32, highway %in% c("trunk_link", "primary",  "tertiary",  "secondary", "secondary_link", "tertiary_link",  "trunk",  "primary_link"))

  acciaieria <- st_read("~/R/terni/data/acciaieria/acciaieria.shp")   # acciaieria
  acciaieria$field_1 <- c("cold_area", "hot_area", "scrapyard")
  
  # dominio ####
  dominio <- st_read(glue("~/R/terni/data/dominio/dominio_{res}m.shp"))
  # dominio <- st_read("~/R/terni/data/dominio/dominio_200m_redux.shp")

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

  df_meteo <- readr::read_csv("~/R/terni/data/dataframes/df_terni_meteo_mensili_periodo.csv", show_col_types = FALSE) %>% 
    arrange(data)

  dists <- c(25, 50, 75, 100, 200) # i buffer da considerare
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



# df <- readr::read_csv("data/dataframes/df_finale_lod_clean.csv", show_col_types = FALSE)
df <- readr::read_csv("data/dataframes/df_finale_raw.csv", show_col_types = FALSE)

modelli <- readRDS("~/R/terni/rds_out/modelli_gaussian_clean.RDS")

index <- grep(pltnt, names(df), value = FALSE)
names(df)[index] <- "value"
source("f_test.R")

gam_tdf <- mgcv::gam(formula(modelli[[pltnt]]), data = df, gamma = 1.4, family = family(modelli[[pltnt]]))


# routine #### 
{
  log_open(file_name = glue::glue("{pltnt}_domine.log"))

  map(1:nrow(dominio), \(id) {
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
    log_print(id, hide_notes = TRUE)
    data.frame("variable" = c(getBufferUA(dist, lista_ua[["s8_sup_200"]], id),
                              getBufferUA(dist, lista_ua[["s6_sup_200"]], id),
                              getAcciaMinDist(dist, id, "cold_area"),
                              getAcciaMinDist(dist, id, "hot_area"),
                              getAcciaMinDist(dist, id, "scrapyard"),
                              getBufferImperv(dist, id),
                              getBufferBH(dist, id), 
                              getBufferIntSEZ(dist, id),
                              getBufferIntStrade(dist, id),
                              getFerroMinDist(dist, id))) %>% t() -> df_spat
    # log_print(df_spat, hide_notes = TRUE)
    rownames(df_spat) <- NULL
    colnames(df_spat) <- c('s8_sup_200', 's6_sup_200', 'cold_area', 'hot_area', 'scrapyard', 'imp_200', 'bh_200', 'pop_200', 'ml_200', 'm_dis_ferr')

    map(df_meteo$data, \(d) {
      cbind(
        filter(df_meteo, data == d),
        df_spat
      ) -> pdf
      # log_print(pdf, hide_notes = FALSE)

      mgcv::predict.gam(gam_tdf, newdata = pdf, type = "response") -> mod
      # log_print(mod, hide_notes = TRUE)
    })
  }) -> ppipp
  
  log_close()
}

saveRDS(ppipp, file = glue::glue("~/R/terni/rds_out_traccianti/{pltnt}_{dist}m_{res}res.RDS"))