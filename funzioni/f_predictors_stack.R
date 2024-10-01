# init e dati ####
{
  rm(list = ls())
  
  library(dplyr)
  library(forcats)
  # library(ggplot2)
  library(glue)
  # library(logr)
  # library(lubridate)
  library(purrr)
  library(sf)
  library(stringr)
  library(terra)
  
  args <- commandArgs(trailingOnly = TRUE)
  
  
  source("~/R/terni/ns_stagioni.R")

  df <- readr::read_csv("~/R/terni/data/dataframes/df_finale_raw.csv", show_col_types = FALSE)
  df <- f_stagioni(df) # applico la definizione delle stagioni

  ferrovia <- st_read("~/R/terni/data/osm/ferrovie.shp")
  
  strade_utm32 <- st_read("~/R/terni/data/osm/strade_interesse.shp") # strade di interesse
  strade_utm32_filtered <- filter(strade_utm32, highway %in% c("trunk_link", "primary",  "tertiary",  "secondary", "secondary_link", "tertiary_link",  "trunk",  "primary_link"))

  acciaieria <- st_read("~/R/terni/data/acciaieria/acciaieria.shp")   # acciaieria
  acciaieria$field_1 <- c("cold_area", "hot_area", "scrapyard")
  
  # dominio ####
  dominio <- st_read(glue("~/R/terni/data/dominio/dominio_100m.shp"))
  dominio_redux <- st_read("~/R/terni/data/shp/dominio_redux_articolo.shp")
  
  terni_sez <- st_read("~/R/terni/data/shp/Terni_sez.shp") # sezioni di censimento
  terni_sez_crop <- st_crop(terni_sez, dominio) 
  
  terni_indicatori_sez <- readr::read_csv("~/R/terni/data/R10_indicatori_2021_sezioni_terni.csv", show_col_types = FALSE)
  terni_sez_pop <- inner_join(terni_sez, select(terni_indicatori_sez, SEZ2011, P1), by = join_by(SEZ2021 == SEZ2011))
  
  
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
  names(lista_ua) <- paste0(cod_str)
  
  terni_ua_fltr <- filter(terni_ua_all, code_2018 %in% codes_2018)
  terni_ua_utm32 <- st_transform(terni_ua_fltr, 32632) # WGS84/UTM 32
  
  imperm <-  terra::rast("~/R/terni/data/tiff/rst_impermeabilizzazione_utm32.tif")
  imper_rst <- as.data.frame(imperm, xy = TRUE)
  
  bh <- rast("~/R/terni/data/bh/Dataset/IT515_TERNI_UA2012_DHM_V010.tif")

  df_meteo <- readr::read_csv("~/R/terni/data/dataframes/df_terni_meteo_mensili_periodo.csv", show_col_types = FALSE) %>% 
    arrange(data) %>% 
    mutate(stagione = case_when(
      month_y %in% c("December_16", "January_17", "February_17", "December_17", "February_18") ~ "I",
      month_y %in% c("March_17", "April_17", "May_17") ~ "P",
      month_y %in% c("July_17", "August_17") ~ "E",
      month_y %in% c("September_17", "November_17") ~ "A"
    ))
  
  dists <- c(25, 50, 75, 100, 200) # i buffer da considerare
  rm(terni_ua_all, terni_sez)
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
# definizione funzioni ####
{
  getBufferUA <- function(dist, code, pt_id) {
    pt_buffer <- st_buffer(filter(dominio, id == pt_id), dist, singleSide = FALSE, nQuadSegs = 17)
    
    var <- filter(terni_ua_utm32, code_2018 == code)
    
    val <- as.numeric( st_area(st_union( st_intersection(pt_buffer, var)  )) )
    if(!length(val) == 0) { 
      return(val)
    }else{ 
      return(0)
    }
  }
  # getBufferUA(100, 12100, 30) # test
  
  # pt_buffer <- st_buffer(filter(dominio, id == 6597), dist, singleSide = FALSE, nQuadSegs = 17)
  # 
  # ggplot() + #geom_sf(data = terni_sez_crop) + 
  #   geom_sf(data = pt_buffer, fill = "red") +
  #   geom_sf(data = st_intersection(pt_buffer, var), fill = "yellow")+
  # geom_sf(data = st_intersection(pt_buffer, var)[1, ], fill = "green")+ 
  #   geom_sf(data = st_intersection(pt_buffer, var)[2, ], fill = "blue")
  # 
  # var <- filter(terni_ua_utm32, code_2018 == 12100)
  # 
  # st_intersection(pt_buffer, var) %>% st_union() -> tmp 
  #   summarise(area = st_area(geom))
    # st_drop_geometry %>%
    # summarise(area = sum(area)) %>% select(area) %>% as.numeric()  
  
  
  getBufferImperv <- function(dist, pt_id) {
    b <- buffer(vect(filter(dominio, id == pt_id)), dist, quadsegs = 17)
    
    extract(imperm, b, xy = TRUE) %>%
      group_by(ID) %>%
      summarise(m = mean(rst_impermeabilizzazione_utm32, na.omit = TRUE)) %>% 
      select(m) %>% as.numeric() -> val
    
    if(!is.na(val)) { return(val)}else{ return(0)}
    # return(val)
  }
  # getBufferImperv(100, 30) # test
  
  # b <- buffer(vect(filter(dominio, id == 7311)), 100, quadsegs = 17)
  # bb <- buffer(vect(dominio[7311, "id"] ), 100, quadsegs = 17)
  # extract(imperm, b, xy = TRUE) %>%
  #   group_by(ID) %>%
  #   summarise(m = mean(rst_impermeabilizzazione_utm32)) %>% 
  #   select(m) %>% as.numeric()
  
  # ggplot(terni_sez_crop) + geom_sf() +
  #   geom_sf(data = filter(dominio, id == 7311), color = "red") +
  #   geom_sf(data = st_as_sf(b), color = "green", fill = "transparent")+
  #   geom_sf(data = st_as_sf(bb), fill = "black") 
  
  
  # # calcola l'altezza media per gli edifici che sono nel buffer
  getBufferBH <- function(dist, pt_id) {
    b <- buffer(vect(filter(dominio, id == pt_id)), dist, quadsegs = 17)
    
   extract(bh, b, xy = TRUE) %>%
      group_by(ID) %>%
      summarise(m = mean(IT515_TERNI_UA2012_DHM_V010, na.rm = TRUE), .groups = 'drop') %>% 
      select(m) %>% as.numeric() %>% round() -> val
    
    if(!is.na(val)) {
      return(val)
    }else{ 
      return(0)
    }
  }
  # getBufferBH(200, 1920) # se non lo trasformiamo noi lo fa in automatico
  
  getDensBHIndex <- function(dist, pt_id) {
    pt_buffer <- st_buffer(filter(dominio, id == pt_id), 100, singleSide = FALSE, nQuadSegs = 17)
    
    cod_int <- c(11100, 11210, 11220, 11230, 11240)
    perc <- c(0.90, 0.65, 0.40, 0.20, 0.05)
    
    inte <- st_intersection(pt_buffer, filter(terni_ua_utm32, code_2018 %in% cod_int))
    
    df_area <- cbind(inte, st_area(inte)) %>%
      st_drop_geometry() %>%
      select(code_2018, st_area.inte.) %>%
      set_names(c("code_2018", "area"))
    
    df_area %>% group_by(code_2018) %>% summarise(area_tot = sum(area)) -> df_area
    
    df_area$code_2018 <- as.numeric(df_area$code_2018)
    
    data.frame(cod_int, perc) %>% set_names(c("code_2018", "perc")) %>%
      inner_join(df_area, by = join_by(code_2018)) %>%
      mutate(index = sum(perc*area_tot)/sum(area_tot) ) %>% dplyr::select(index) -> index
    
    unique(index) %>% as.numeric() -> val
    if(!is.na(val)) {
      return(val)
    }else{ 
      return(0)
    }
  }
  # getDensBHIndex(100, 2900)
  
  getBufferIntSEZ <- function(dist, pt_id) {
    pt_buffer <- st_buffer(filter(dominio, id == pt_id), dist, singleSide = FALSE, nQuadSegs = 17)
    
    st_intersection(terni_sez_pop, pt_buffer) %>%
      mutate(area_int = as.numeric( st_area(geometry)) ) %>%
      mutate(PP1 = as.integer( (area_int / SHAPE_Area) * P1)) %>%
      select(SHAPE_Area, area_int, P1, PP1) %>%
      st_drop_geometry() %>%
      summarise(m = sum(PP1)) %>% select(m) %>% as.numeric() %>% round()
  }
  # getBufferIntSEZ(200, 1920)
  
  getBufferIntStrade <- function(dist, pt_id) {
    pt_buffer <- st_buffer(filter(dominio, id == pt_id), dist, singleSide = FALSE, nQuadSegs = 17)
    
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
  
  
  getStradeMinDist <- function(dist, pt_id) {
    return(
      as.numeric(min(st_distance(filter(dominio, id == pt_id), strade_utm32_filtered)) %>% round())
    )
  }
  # st_distance(dominio[1925, "id"], strade_utm32_filtered) -> df
  # getStradeMinDist(200,1925)
  
  getFerroMinDist <- function(dist, pt_id) {
    return(
      as.numeric(min(st_distance(filter(dominio, id == pt_id), ferrovia)) %>% round())
    )
  }
  
  getAcciaMinDist <- function(dist, pt_id, var) {
    return(
      as.numeric(min(st_distance(filter(dominio, id == pt_id), filter(acciaieria, field_1 == var) ) ) ) %>% round()
    )
  }
  
  
  getBufferRastKNDVI <- function(dist, rst, mese, pt_id) {
    b <- buffer(vect(filter(dominio, id == pt_id)), dist, quadsegs = 17)
    
    extract(rst, b, xy = TRUE) %>%
      group_by(ID) %>%
      mutate(ifelse(get(mese) < 0, 0, get(mese) )) %>%
      summarise(m = mean(get(mese), na.rm = TRUE), .groups = 'drop') %>%
      setNames(c("ID", "media", "site")) %>% select(media) %>% as.numeric() 
  }
}


# routine #### 
{
  map(dominio$id, \(id) {
    print(id)
    data.frame("variable" = c(
      getBufferUA(200, lista_ua[["s8"]], id),
      getBufferUA(200, lista_ua[["s6"]], id),
      getAcciaMinDist(200, id, "cold_area"),
      getAcciaMinDist(200, id, "hot_area"),
      getAcciaMinDist(200, id, "scrapyard"),
      getBufferImperv(200, id),
      getDensBHIndex(200, id),
      getBufferIntSEZ(200, id),
      getBufferIntStrade(200, id),
      getFerroMinDist(200, id),
      getStradeMinDist(200, id)
    )
    ) %>% t() -> df_spat
    
    rownames(df_spat) <- NULL
    colnames(df_spat) <- c(
      's8_sup_200', 's6_sup_200', 
      'cold_area', 'hot_area', 'scrapyard', 
      'imp_200', 
      'bh_200', 
      'pop_200', 
      'ml_200', 
      'm_dis_ferr',
      'min_d')

    cod <- as.numeric(id)
    
    map(df_meteo$data, \(d) {
      cbind(
        filter(df_meteo, data == d),
        df_spat,
        dominio %>% filter(id == cod) %>% st_coordinates()
      ) -> pdf
      
    })
    
  }) -> mdf
  
}

# saveRDS(mdf, file = "~/R/terni/data/predittori_raster_stack.rds")

