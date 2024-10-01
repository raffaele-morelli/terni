# predict raster #######
# init ####
{
  library(dplyr)
  library(ggplot2)
  library(ggspatial)
  library(purrr)
  library(raster)
  library(sf)
  
  source("~/R/terni/ns_stagioni.R")
  
  dominio_redux <- st_read("~/R/terni/data/shp/dominio_redux_articolo.shp")
  dominio <- st_read("~/R/terni/data/dominio/dominio_100m.shp") # 109 col
  
  strade_utm32 <- st_read("~/R/terni/data/osm/strade_interesse.shp") # strade di interesse
  pt_misura_utm32 <- st_read("~/R/terni/data/shp/punti_misura.shp") 
  acciaieria <- st_read("~/R/terni/data/acciaieria/acciaieria.shp")   # acciaieria
  
  
  mdf <- readRDS(file = "~/R/terni/data/predittori_raster_stack.rds")
}

mdf %>% length() # i predittori su tutti i punti
mdf[[1]] %>% length() # predittori sul punto id=1 per i 12 periodi di campionamento
mdf[[1]][[1]] %>% length() # i predittori per il periodo 1


map(mdf, \(x) {
  x[[1]] # il primo periodo di campionamento per tutti i punti
}) -> tmp


{
  pltnt <- "Cr_i"
  titolo <- pltnt
  df <- readr::read_csv("~/R/terni/data/dataframes/df_finale_raw.csv", show_col_types = FALSE)
  df <- f_stagioni(df)
  
  modelli <- readRDS(glue::glue("~/R/terni/rds_gaussian_test9/modelli_test9_clean.RDS"))
  index <- grep(pltnt, names(df), value = FALSE)
  names(df)[index] <- "value"
  
  # modello di training 
  mod <- mgcv::gam(
    formula(modelli[[pltnt]]),
    data = df,
    gamma = 1.4,
    family = family(modelli[[pltnt]])
  )
}

df_raster <- do.call(rbind, tmp)
df_sf <- st_as_sf(df_raster, coords = c("X", "Y"), crs = 32632)


# calcolo contributo ############
{
  library(mgcv)
  intercetta <- as.numeric(mod$coefficients[1])
  
  Xp <- predict.gam(mod, df_sf, type = "lpmatrix")
  jd_indxs <- grep("cold_area|hot_area", colnames(Xp))
  
  beta <- coef(mod)
  b_indxs <- grep("cold_area|hot_area", names(beta))
  
  preds <- as.vector(Xp[,jd_indxs] %*% beta[b_indxs])
  
  df_l <- cbind(exp(preds + intercetta))
  
  names(df_l)[ncol(df_l)] <- "norm"
  
  # df_l <- df_l %>% mutate(date = as.Date(jd, origin = as.Date("1970-01-01")))
  
  acc <- Xp[,grep("cold_area|hot_area", colnames(Xp), invert = TRUE)] %*% beta[grep("cold_area|hot_area", colnames(Xp), invert = TRUE)]
  acc <- exp(acc - intercetta)
  
  df_l <- cbind(df_l, acc)
}

dim(df_l)

{
  m <- matrix(df_l[,1]+df_l[,2], ncol = 109,  byrow = FALSE)
  
  r <- raster::raster(m)
  
  bbox <- st_bbox(dominio) 
  r_extent <- c(as.numeric(bbox["xmin"]), as.numeric(bbox["xmax"]), as.numeric(bbox["ymin"]), as.numeric(bbox["ymax"]))
  
  raster::extent(r) <- r_extent
  crs(r) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
  
  r_crop <- raster::crop(r, dominio_redux)
  
  # plot(r)
  
  r_df <- as.data.frame(r_crop, xy = TRUE) %>%
    na.omit() %>%
    setNames(c("x", "y", "value"))
  
  ggplot(data = r_df) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    geom_sf(data = st_crop(strade_utm32, st_bbox(dominio_redux)), color = "grey80", fill = "transparent", size = 0.5) +
    geom_sf(data = pt_misura_utm32, shape = 21, fill = "dodgerblue", color = "black", size = 2) +
    geom_sf(data = filter(acciaieria, field_1 != "scrapyard"), shape = 8, fill = "dodgerblue4", color = "black", size = 2) +
    # geom_sf_text(data = pt_misura_utm32, aes(label = Site), color = "black", nudge_x = -80, nudge_y = -125, size = 3) +
    # scale_fill_viridis_c(direction = -1, option = "magma") +
    scale_fill_distiller(palette = "Spectral", direction = -1, name = titolo) +
    coord_sf(datum = sf::st_crs(32632)) +
    # annotation_scale(location = "br", width_hint = 0.25, pad_y = unit(0.75, "in"), pad_x = unit(1, "in")) +
    # annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(1.25, "in"), pad_y = unit(1, "in"), style = north_arrow_fancy_orienteering) +
    theme_void() +
    theme(legend.position = "right", plot.margin = unit(c(1,1,1,1), "cm")) 
}
