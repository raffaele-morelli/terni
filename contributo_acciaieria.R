# predict raster #######

# init ####
{
  library(dplyr)
  library(ggplot2)
  library(ggspatial)
  library(mgcv)
  library(purrr)
  # library(raster)
  library(terra)
  library(readr)
  library(sf)
  library(stringr)
  
  source("~/R/terni/ns_stagioni.R")
  
  dominio_redux <- st_read("~/R/terni/data/shp/dominio_redux_articolo.shp")
  dominio <- st_read("~/R/terni/data/dominio/dominio_100m.shp") # 109 col
  
  strade_utm32 <- st_read("~/R/terni/data/osm/strade_interesse.shp") # strade di interesse
  pt_misura_utm32 <- st_read("~/R/terni/data/shp/punti_misura.shp") 
  acciaieria <- st_read("~/R/terni/data/acciaieria/acciaieria.shp")   # acciaieria
  
  traccianti_acciaieria <- read_csv("data/traccianti_acciaieria.csv", col_names = FALSE, show_col_types = FALSE) %>% pull() # traccianti acciaeria
  
  mdf <- readRDS(file = "~/R/terni/data/predittori_raster_stack.rds")
  
  modelli <- readRDS(glue::glue("~/R/terni/rds_gaussian_test9/modelli_test9_clean.RDS"))
  
  df <- readr::read_csv("~/R/terni/data/dataframes/df_finale_raw.csv", show_col_types = FALSE)
  df <- f_stagioni(df)
}

# mdf %>% length() # i predittori su tutti i punti
# mdf[[1]] %>% length() # predittori sul punto id=1 per i 12 periodi di campionamento
# mdf[[1]][[1]] %>% length() # i predittori per il periodo 1

walk(traccianti_acciaieria, \(pltnt) {
  writeLines(pltnt)
  
  index <- str_which(pltnt, names(df))
  
  names(df)[index] <- "value"
  
  # modello
  mod <- mgcv::gam(
    formula(modelli[[pltnt]]),
    data = df,
    gamma = 1.4,
    family = family(modelli[[pltnt]])
  )
  
  walk(1:12, \(periodo) {
    
    map(mdf, \(punti) {
      punti[[periodo]] #
    }) -> df_p
    
    df_raster <- do.call(rbind, df_p)
    df_sf <- st_as_sf(df_raster, coords = c("X", "Y"), crs = 32632)
    
    # calcolo contributo ############
    {
      intercetta <- as.numeric(mod$coefficients[1])
      
      Xp <- predict.gam(mod, df_sf, type = "lpmatrix")
      jd_indxs <- grep("cold_area|hot_area", colnames(Xp))
      
      beta <- coef(mod)
      b_indxs <- grep("cold_area|hot_area", names(beta))
      
      preds <- as.vector(Xp[,jd_indxs] %*% beta[b_indxs])
      
      df_l <- cbind(exp(preds + intercetta))
      
      names(df_l)[ncol(df_l)] <- "norm"
      
      # df_l <- df_l %>% mutate(date = as.Date(jd, origin = as.Date("1970-01-01")))
      
      n_acc <- Xp[,grep("cold_area|hot_area", colnames(Xp), invert = TRUE)] %*% beta[grep("cold_area|hot_area", colnames(Xp), invert = TRUE)]
      n_acc <- exp(n_acc - intercetta)
      
      df_l <- cbind(df_l, n_acc)
      
      m <- matrix(df_l[,1], ncol = 109,  byrow = FALSE)
      
      r <- terra::rast(m)
      
      bbox <- st_bbox(dominio) 
      r_extent <- c(as.numeric(bbox["xmin"]), as.numeric(bbox["xmax"]), as.numeric(bbox["ymin"]), as.numeric(bbox["ymax"]))
      
      terra::ext(r) <- r_extent
      terra::crs(r) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
      
      r_crop <- terra::crop(r, dominio_redux)
      
      periodo <- str_pad(periodo, 2, pad = "0")
      
      terra::writeRaster(r_crop, glue::glue('~/R/terni/tiff_out/acciaieria/{pltnt}_{periodo}.tif'), overwrite = TRUE)
    }
  })
})

