## esempi di plot 

# ggplot(data = imper_rst) + 
#   geom_raster(aes(x = x, y = y, fill = impermeabilizzazione_utm32)) +
#   scale_fill_viridis_c() + theme_void() + coord_fixed()


# ggplot() + 
#   # geom_sf(data = com) +
#   # geom_sf(data = filter(terni_utm32, code_2018 == 12210)) +
#   geom_sf(data = filter(terni_utm32, code_2018 == 12220), color = alpha("grey", 0.2)) +
#   geom_raster(data = imper_rst, aes(x = x, y = y, fill = impermeabilizzazione_utm32), alpha = 0.5) +
#   scale_fill_viridis_c() +
#   geom_sf(data = st_buffer(pt_misura, 200), color = "green") +
#   geom_sf(data = pt_misura, color = "red", size = 0.5) +
#   geom_sf(data = i_12220, fill = "violet") +
#   coord_sf(xlim = c(794643.0, 801954.6), ylim = c(4717144.4, 4720794.3)) +
#   theme_void()
