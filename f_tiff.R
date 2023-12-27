
# pltnt <- "Cr_i"
mese <- 1

dominio <- st_read("~/R/terni/data/dominio/dominio_200m.shp")
st_bbox(dominio) -> bbox

fls <- list.files("~/R/terni/rds_out_traccianti/", full.names = TRUE)

walk(fls, \(f) {
  tools::file_path_sans_ext(f) %>% basename() -> fout
  trcnt <- readRDS(f)
  trcnt_df <- do.call(rbind.data.frame, trcnt)
  
  r <- matrix(trcnt_df[,mese], ncol = 54,  byrow = FALSE) %>% raster::raster()
  
  raster::extent(r) <- c(as.numeric( bbox["xmin"] ), as.numeric( bbox["xmax"] ),  as.numeric( bbox["ymin"] ), as.numeric( bbox["ymax"] ) )
  r <- writeRaster(r, glue::glue('~/R/terni/tiff_out/{fout}_{mese}.tif'), overwrite = TRUE)
})


plot(r)
r_df <- as.data.frame(r, xy = TRUE) %>%
  na.omit() %>%
  setNames(c("x", "y", "value"))

ggplot(data = r_df) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c(direction = -1) +
  theme_void() +
  theme(
    legend.position = "left"
  ) + coord_equal()

