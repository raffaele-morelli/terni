{
  # library(sf)
  library(dplyr)
  library(tidyverse)
  library(readr)
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

fls <- list.files(path = "~/R/terni/data", pattern = "^df_", full.names = TRUE)
bdata <- lapply(fls, function(x) {
  read_csv(x)
})
names(bdata) <- basename(fls) %>% str_remove_all(pattern = "df_") %>% str_remove_all(".csv")

bdata[['acciaieria']]
bdata[['popolazione']]
bdata