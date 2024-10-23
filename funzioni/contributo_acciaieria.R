# predict raster 

rm(list = ls())

# init ####
{
  library(dplyr)
  # library(ggplot2)
  library(glue)
  library(ggspatial)
  library(mgcv)
  library(purrr)
  library(terra)
  library(readr)
  library(sf)
  library(stringr)
  
  source("~/R/terni/ns_stagioni.R")
  
  dominio_redux <- st_read("~/R/terni/data/shp/dominio_redux_articolo.shp")
  dominio <- st_read("~/R/terni/data/dominio/dominio_100m.shp") # 109 col

  # traccianti_acciaieria <- read_csv("data/traccianti_acciaieria.csv", col_names = FALSE, show_col_types = FALSE) %>% pull() # traccianti acciaeria
  traccianti_acciaieria <-  c("Cr_i", "Mo_s", "Ni_i", "W_s")
  
  mdf <- readRDS(file = "~/R/terni/data/predittori_raster_stack.rds")
  
  modelli <- readRDS(glue::glue("~/R/terni/rds_gaussian_test9/modelli_test9_clean.RDS"))
  
  df <- readr::read_csv("~/R/terni/data/dataframes/df_finale_raw.csv", show_col_types = FALSE)
  df <- f_stagioni(df)
  
  met <- "test11"
  
  tiff_dir <- glue("tiff_out_improved_{met}")
  hua <- FALSE
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
  intercetta <- as.numeric(mod$coefficients[1])
  

  walk(1:1, \(periodo) {
    
    df_p <- map(mdf, ~.[[periodo]]) 
    
    df_raster <- do.call(rbind, df_p)
    
    df_sf <- st_as_sf(df_raster, coords = c("X", "Y"), crs = 32632)
    
    # calcolo contributo ############
    {
      Xp <- predict(mod, df_sf, type = "lpmatrix")
      # Then, given that matrix you can do a matrix multiplication with it and the vector of model coefficients (extracted by
      
      beta <- coef(mod)

      # to yield predicted values at the covariate values of x that you requested:
      acc <- Xp[,grep("cold_area|hot_area", colnames(Xp), invert = FALSE)] %*% beta[grep("cold_area|hot_area", colnames(Xp), invert = FALSE)]
      
      # and back-transform if using a non-identity link via the inverse link function #
      
      if(hua == TRUE) {
        contr_acc <- (exp(acc)- 1)*exp(intercetta) # HUA
      }else{
        contr_acc <- exp(acc) # non HUA
      }
      
      df_contr <- tibble(assoluto = contr_acc)

      m_ass <- matrix(df_contr$assoluto, ncol = 109,  byrow = FALSE)
      
      r_ass <- rast(m_ass)

      bbox <- st_bbox(dominio)
      r_extent <- c(as.numeric(bbox["xmin"]), as.numeric(bbox["xmax"]), as.numeric(bbox["ymin"]), as.numeric(bbox["ymax"]))
      
      ext(r_ass) <- r_extent
      crs(r_ass) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
      
      r_crop <- crop(r_ass, dominio_redux)
      
      periodo <- str_pad(periodo, 2, pad = "0")
      
      if(hua == TRUE) {
        path <- glue('~/R/terni/{tiff_dir}/acciaieria/HUA')
        filename <- glue('{path}/{pltnt}_steel_plan_HUA.tif')
      }else{
        path <- glue('~/R/terni/{tiff_dir}/acciaieria/non-HUA')
        filename <- glue('{path}/{pltnt}_steel_plan_non-HUA.tif')
      }
      
      # writeLines(filename)
      
      dir.create(path = path, recursive = TRUE, showWarnings = F)
      
      terra::writeRaster(r_crop,
                         filename = filename, 
                         overwrite = TRUE)
    }
  })
})
