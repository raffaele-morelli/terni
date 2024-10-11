# predict raster 

rm(list = ls())
# init ####
{
  library(dplyr)
  library(ggplot2)
  library(ggspatial)
  library(glue)
  library(mgcv)
  library(purrr)
  library(terra)
  library(readr)
  library(sf)
  library(stringr)
  
  source("~/R/terni/ns_stagioni.R")
  
  dominio_redux <- st_read("~/R/terni/data/shp/dominio_redux_articolo.shp")
  dominio <- st_read("~/R/terni/data/dominio/dominio_100m.shp") # 109 col

  st_bbox(dominio) -> bbox
  r_extent <- c(as.numeric(bbox["xmin"]), as.numeric(bbox["xmax"]), as.numeric(bbox["ymin"]), as.numeric(bbox["ymax"]))
  
  traccianti <- read_csv("~/R/terni/data/selezione_terni.csv", col_names = FALSE, show_col_types = FALSE) %>% pull() # traccianti acciaeria
  
  mdf <- readRDS(file = "~/R/terni/data/predittori_raster_stack.rds")
  
  modelli <- readRDS(glue::glue("~/R/terni/rds_gaussian_test9/modelli_test9_clean.RDS"))
  
  df <- read_csv("~/R/terni/data/dataframes/df_finale_raw.csv", show_col_types = FALSE)
  df <- f_stagioni(df)
  
  tiff_dir <- "tiff_out_improved"
}

# mdf %>% length() # i predittori su tutti i punti
# mdf[[1]] %>% length() # predittori sul punto id=1 per i 12 periodi di campionamento
# mdf[[1]][[1]] %>% length() # i predittori per il periodo 1

# siamo interessati a produrre dei raster. Non ci occorre salvare il risultato in un oggetto
walk(pull(traccianti), \(pltnt) {
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
    
    # iterazione su mdf per estrarre il dataframe delle covariate
    # corrispondente al periodo
    df_p <- map(mdf, ~.[[periodo]]) 
    
    df_raster <- do.call(rbind, df_p)
    df_sf <- st_as_sf(df_raster, coords = c("X", "Y"), crs = 32632)
    
    # predict ############
    {
      mod <- predict(mod, df_sf, type = "response")
      # mod <- predict(mod, df_raster, type = "response")
      
      m <- matrix(mod, ncol = 109,  byrow = FALSE)
      r <- terra::rast(m)
      
      periodo <- str_pad(periodo, 2, pad = "0")
      
      terra::ext(r) <- r_extent
      crs(r) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
      
      r_crop <- terra::crop(r, dominio_redux)
      
      dir.create(glue::glue('~/R/terni/{tiff_dir}/{pltnt}'), showWarnings = F)
      
      writeRaster(r_crop,
                  glue::glue('~/R/terni/{tiff_dir}/{pltnt}/{pltnt}_{periodo}.tif'),
                  overwrite = TRUE)
    }
  })
})

# tiff media annuale ####
walk(pull(traccianti), \(t) {
  writeLines(t)
  
  fs <- list.files(glue("~/R/terni/{tiff_dir}/{t}"),  full.names = T)
  rs <- terra::rast(fs)
  
  pesi <- c(.060, .068, .078, .078, .078, .078, .096, .096, .078, .078, .133, .078)
  
  rsw <- weighted.mean(rs, w = pesi)
  # plot(rsw)
  
  writeRaster(rsw, glue("~/R/terni/{tiff_dir}/year/{t}_mean.tif"), overwrite = TRUE)
})


# tiff stagione ####
stagioni <- c("01|02|03|11|12", "04|05|06", "07|08", "09|10")

walk(pull(traccianti), \(t) {
  writeLines(t)
  
  walk(stagioni, \(s) {
    path <- glue("~/R/terni/{tiff_dir}/{t}/")
    pattern <- glue("^{t}_({s})")

    fs <- list.files(path = path, 
                     pattern = pattern, 
                     recursive = T,
                     full.names = T)
    
    # cat(fs, sep = "\n")
    rs <- rast(fs) # crea un raster stack
    # plot(rs)
    
    pesi <- c(0.060, 0.068, 0.078, 0.078, 0.078, 0.078, 0.096, 0.096, 0.078, 0.078, 0.133, 0.078)
    rsw <- weighted.mean(rs, w = pesi)
    
    r <- mean(rsw)
    # plot(r)
    
    stagione <- case_when(s == "01|02|03|11|12" ~ "Winter", 
                          s == "04|05|06" ~ "Spring", 
                          s == "07|08" ~ "Summer", 
                          s == "09|10" ~ "Autumn")
    names(r) <- stagione
    
    dir.create(glue("~/R/terni/{tiff_dir}/{t}/season/"), showWarnings = FALSE)
    writeRaster(r, glue("~/R/terni/{tiff_dir}/{t}/season/{t}_{stagione}.tif"), overwrite = TRUE)
  })
})
