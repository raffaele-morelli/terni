# init ####
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
  library(reshape2)
  # library(mapview)
  
  # library(lattice)
  # library(RColorBrewer)
  
  pt_misura <- st_read("~/R/terni_asi/data/shp/punti_misura.shp")
  
  sites <- pt_misura$Site
  dists <- c(25, 50, 75, 100, 200) # i buffer da considerare  
} 

fls <- list.files(path = "~/R/terni_asi/data/dataframes", pattern = "^df_", full.names = TRUE)
bdata <- lapply(fls, function(x) {
  read_csv(x)
})
names(bdata) <- basename(fls) %>% str_remove_all(pattern = "df_") %>% str_remove_all(".csv")

# keep_by_name <- function(l, keep_names) l[keep_names]

# do.call( cbind, bdata[c(grep("^lai", names(bdata)), grep("^ndvi", names(bdata)), grep("^kndvi", names(bdata)) ) ] ) -> tmp
# indxs <- grep("Site", names(tmp))
# 
# big_df <- tmp %>% 
#   select(-c(indxs)) %>% 
#   cbind(sites)
# 
# library(corrplot)
# library(reshape2)
# 
# filter(big_df, sites == "RI") %>% melt(id.vars = "sites") -> m_tmp
# 
# listone <- list()
# for (i in c("ndvi", "kndvi", "lai")) {
#   for (j in dists) {
#     listone[[i]] <- m_tmp %>% filter(!grepl(glue::glue("^{i}"), variable))
#   }
# }

# approccio 2 ####
outdir <- "~/R/terni_asi/data/dataframes"
test <- list()

estrai <- function(var, site) {
  print(var)

  fls <- list.files(outdir, pattern = glue("^df_{var}"), full.names = TRUE)
  
  app <- list()
  lapply(fls, function(f) {
    read_csv(f, show_col_types = FALSE) %>% 
      melt(id.vars = 'pt_misura$Site') %>% 
      filter(`pt_misura$Site` == site) %>% 
      dplyr::select(value) %>% setNames(c("value"))
  }) -> app
  
  c_names <- stri_paste(dists)
  
  df <- do.call(cbind, app) %>% setNames(c_names)
  print(df)
  test[["var"]] <- df
}
# estrai("kndvi", "PI") %>% as.matrix() %>% cor() %>% corrplot()
# estrai("ndvi", "PI") %>% as.matrix() %>% cor() %>% corrplot()
# estrai("lai", "PI") %>% as.matrix() %>% cor() %>% corrplot()

appoggio <<- list()
for (i in c("kndvi","ndvi", "lai") ) {
  appoggio[[i]] <-  estrai(i, "RI") 
} 
# indici di vegetazione e area fogliare ####
do.call(cbind, appoggio) -> df_indici

# meteo ####
df_terni_meteo_mensili <- read_csv("data/dataframes/df_terni_meteo_mensili.csv") %>% 
  head(n = -1)

# cbind(df_indici, df_terni_meteo_mensili) %>% 
#   dplyr::select(-data) %>% 
#   as.matrix() %>% 
#   cor() %>% 
#   corrplot::corrplot()

# imperviousness ####
df_imperviousness <- read_csv("data/dataframes/df_imperviousness.csv")
names(df_imperviousness) <- paste("im", colnames(df_imperviousness), sep = "_")

# building_heights ####
df_building_heights <- read_csv("data/dataframes/df_building_heights.csv")
names(df_building_heights) <- paste("bh", colnames(df_building_heights), sep = "_")
