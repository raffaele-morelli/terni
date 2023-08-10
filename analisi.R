{
  library(sf)
  library(dplyr)
  # library(tidyverse)
  library(readr)
  library(stringr)
  library(stringi)
  # library(ggplot2)
  # library(plotly)
  # library(terra)
  # library(ncdf4) # package for netcdf manipulation
  # library(raster) # package for raster manipulation
  # library(rgdal) # package for geospatial analysis
  # library(chron)
  # library(readxl)
  library(glue)
  # library(mapview)
  
  # library(lattice)
  # library(RColorBrewer)
} 

{
  terni_all <- st_read("~/R/terni/data/IT515L2_TERNI_UA2018_v013/Data/IT515L2_TERNI_UA2018_v013.gpkg")
  terni_sez <- st_read("~/R/terni/data/shp/Terni_sez.shp")
  terni_indicatori_sez <- read_csv("data/R10_indicatori_2021_sezioni_terni.csv")
  
  pt_misura <- st_read("~/R/terni/data/shp/punti_misura.shp")
  
  # variabili di interesse
  terni_fltr <- filter(terni_all, code_2018 %in% c(11100, 11210, 11220, 11230, 11240, 12100, 12210, 12220))
  
  terni_utm32 <- st_transform(terni_fltr, 32632) # WGS84/UTM 32
  
  # imperm <- rast("~/R/terni/data/tiff/rst_impermeabilizzazione_utm32.tif")
  # imper_rst <- as.data.frame(imperm, xy = TRUE)
  
  dists <- c(25, 50, 75, 100, 200) # i buffer da considerare
}

fls <- list.files(path = "~/R/terni/data/dataframes", pattern = "^df_", full.names = TRUE)
bdata <- lapply(fls, function(x) {
  read_csv(x)
})
names(bdata) <- basename(fls) %>% str_remove_all(pattern = "df_") %>% str_remove_all(".csv")

pt_misura <- st_read("~/R/terni/data/shp/punti_misura.shp")

sites <- pt_misura$Site
dists <- c(25, 50, 75, 100, 200) # i buffer da considerare

# keep_by_name <- function(l, keep_names) l[keep_names]

do.call( cbind, bdata[c(grep("^lai", names(bdata)), grep("^ndvi", names(bdata)), grep("^kndvi", names(bdata)) ) ] ) -> tmp
indxs <- grep("Site", names(tmp))

big_df <- tmp %>% 
  select(-c(indxs)) %>% 
  cbind(sites)

library(corrplot)
library(reshape2)

filter(big_df, sites == "RI") %>% melt(id.vars = "sites") -> m_tmp

listone <- list()
for (i in c("ndvi", "kndvi", "lai")) {
  for (j in dists) {
    listone[[i]] <- m_tmp %>% filter(!grepl(glue::glue("^{i}"), variable))
  }
}

# approccio 2 ####
outdir <- "~/R/terni/data/dataframes"

estrai <- function(var, site) {
  fls <- list.files(outdir, pattern = glue("^df_{var}"), full.names = TRUE)
  
  app <- list()
  lapply(fls, function(f) {
    read_csv(f) %>% 
      melt(id.vars = 'pt_misura$Site') %>% 
      filter(`pt_misura$Site` == site) %>% 
      dplyr::select(value) %>% setNames(c("value"))
  }) -> app
  c_names <- stri_paste(var, dists)
  
  df <- do.call(cbind, app) %>% setNames(c_names)  
  return(df)
}
estrai("kndvi", "PI") %>% as.matrix() %>% cor() %>% corrplot()
estrai("ndvi", "PI") %>% as.matrix() %>% cor() %>% corrplot()
estrai("lai", "PI") %>% as.matrix() %>% cor() %>% corrplot()

