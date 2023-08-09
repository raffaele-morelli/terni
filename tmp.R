## esempi di plot 

# impermeabilizzazione ####
ggplot() +
  geom_raster(data = imper_rst, aes(x = x, y = y, fill = rst_impermeabilizzazione_utm32)) +
  scale_fill_viridis_c() 


# building ####
# library(tidyterra)
as.data.frame(bh, xy = TRUE) %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = IT515_TERNI_UA2012_DHM_V010)) +
  geom_spatvector(data = v, color = "red") +
  scale_fill_viridis_c() + theme_void()

# sezioni ####
# ggplot(terni_sez_pop) + geom_sf() + geom_sf_label(aes(label = P1), size = 2) 

# osm ####
{
  pt_buffer <- st_buffer(pt_misura, 200, singleSide = FALSE, nQuadSegs = 17)
  # nQuadSegs: con il tuning di questo par si può far corrispondere il buff con quello di arcGIS
  st_intersection(pt_buffer, strade_utm32) -> tmp_inters
  
  tmp_inters %>% 
    group_by(Site) %>% 
    summarise(m = sum(st_length(geometry))) %>% 
    st_cast() %>% 
    mapview()
  
  tmp_inters %>% group_by(Site) %>% summarise(l = st_length(geometry))
  
  ggplot() + geom_sf(data = tmp_inters) + geom_sf(data = pt_misura, color = "red")
}

# ggplot() + geom_sf(data = strade_utm32) 
  # geom_sf(data = pt_buffer, color = "red", fill = "orange" )+
  # coord_sf(datum = st_crs(32632), xlim = c(794643.0, 801954.6), ylim = c(4716044.4, 4720994.3))


# tutto ####
# ggplot() +
#   # geom_sf(data = com) +
#   # geom_sf(data = filter(terni_utm32, code_2018 == 12210)) +
#   # geom_raster(data = imper_rst, aes(x = x, y = y, fill = rst_impermeabilizzazione_utm32)) +
#   geom_sf(data = strade_utm32, color = alpha("black", 1) ) +
#   # geom_sf(data = filter(terni_utm32, code_2018 == 12220), color = alpha("grey", 0.2)) +
#   scale_fill_viridis_c(alpha = 0.2, direction = 1, option = "plasma") +
#   geom_sf(data = st_buffer(pt_misura, 100), color = "dodgerblue", fill = "transparent", lwd = 2) +
#   # geom_sf(data = pt_misura, color = "red", size = 0.5) +
#   coord_sf(datum = st_crs(32632), xlim = c(794643.0, 801954.6), ylim = c(4716044.4, 4720994.3)) +
#   theme_void()
