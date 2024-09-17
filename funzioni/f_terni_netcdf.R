# init 
{
  library(ncdf4) # package for netcdf manipulation
  library(raster) # package for raster manipulation
  library(rgdal) # package for geospatial analysis
  library(ggplot2) # package for plotting
  library(dplyr)
  library(chron)
  library(lattice)
  library(RColorBrewer)
  library(sf)
}


# secondo set ####
list.files(path = "./data/ndvi", full.names = TRUE) 

nc_data <- nc_open("./data/ndvi/T33TUH_201611_201801_S2_L3B_10m_NDVI_monthly_Terni.nc")


# Save the print(nc) dump to a text file
{
  sink('metadata.txt')
  nc_data$var
  nc_data$nvars
  
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "x")
nlon <- dim(lon)

lat <- ncvar_get(nc_data, "y", verbose = F)
nlat <- dim(lat)

print(c(nlon,nlat))

time <- ncvar_get(nc_data, "time")
tunits <- ncatt_get(nc_data, "time", "units")
nt <- dim(time)


pol_st <- stack("./data/ndvi/T33TUH_201611_201801_S2_L3B_10m_NDVI_monthly_Terni.nc")
plot(pol_st)

for (i in names(pol_st)) {
  # ggplot(pol_st[[i]]) 
  print(i)
  writeRaster(ndvi_rasterone[[i]] , glue::glue("tmp/{i}.tiff"), format = 'GTiff', overwrite = T)

}
brick(pol_st) -> ndvi_rasterone
nlayers(ndvi_rasterone)
names(ndvi_rasterone)

