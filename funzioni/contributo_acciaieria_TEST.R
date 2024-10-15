# predict raster 

rm(list = ls())
graphics.off()

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
  library(scam)
  library(sf)
  library(stringr)
  
  source("~/R/terni/ns_stagioni.R")
  source("~/R/terni/funzioni/f_makeSpline.R")
  
  
  dominio_redux <- st_read("~/R/terni/data/shp/dominio_redux_articolo.shp")
  dominio <- st_read("~/R/terni/data/dominio/dominio_100m.shp") # 109 col
  
  # strade_utm32 <- st_read("~/R/terni/data/osm/strade_interesse.shp") # strade di interesse
  # pt_misura_utm32 <- st_read("~/R/terni/data/shp/punti_misura.shp") 
  # acciaieria <- st_read("~/R/terni/data/acciaieria/acciaieria.shp")   # acciaieria
  
  traccianti_acciaieria <- read_csv("data/traccianti_acciaieria.csv", col_names = FALSE, show_col_types = FALSE) %>% pull() # traccianti acciaeria
  
  mdf <- readRDS(file = "~/R/terni/data/predittori_raster_stack.rds")
  
  modelli <- readRDS(glue::glue("~/R/terni/rds_gaussian_test9/modelli_test9_clean.RDS"))
  
  df <- readr::read_csv("~/R/terni/data/dataframes/df_finale_raw.csv", show_col_types = FALSE)
  df <- f_stagioni(df)
}

pltnt <- "Mo_s"
periodo <- 1

writeLines(pltnt)

index <- str_which(pltnt, names(df))

names(df)[index] <- "value"

# modello originale ####
# value ~ s(cold_area, k = 1) + s(u10m_mean) + s(ml_200, k = 1) + s(imp_200, k = 1) + s(min_d, k = 1) + s(m_dis_ferr, k = 1)
{
  df %>% 
    filter(month_y == "December_16") -> df_t
  
  mod <- mgcv::gam(
    value ~ s(cold_area) +  s(ml_200),
    # value ~ cold_area,
    data = df_t,
    gamma = 1.4,
    family = gaussian(link = log)
  )
  summary(mod)
  gratia::draw(mod, residuals = T)
}

# scam #####
{
  mods <-  scam(value ~ s(cold_area, bs = 'po') + s(ml_200, bs = 'po'),
                data = df,
                family = gaussian(link = log))
  
  summary(mods)
  plot(mods)
}

# tweaks #########
{
  knots <- data.frame(x = seq(0, 6000, length = 9)) ## create knots
  
  sm <- smoothCon(s(cold_area, k = 9), df, knots = knots)[[1]]
  
  ## 3rd parameter is value of spline at knot location 0,
  ## set it to 0 by dropping...
  X <- sm$X[,-3]               ## spline basis
  S <- sm$S[[1]][-3,-3]        ## spline penalty
  off <- df$value*0 + 60       ## offset term to force curve through (0, .6)
  
  mod_t <- gam(
    value ~ X + offset(off),
    paraPen = list(X = list(S)),
    data = df
  )

  plot(df$cold_area, df$value)
  lines(df$cold_area, predict(mod_t), col = "red")
  lines(df$cold_area, predict(mod), col = "green")
  
  gratia::draw(mod_t)
}


map(mdf, \(punti) {
  punti[[periodo]] #
}) -> df_p

df_raster <- do.call(rbind, df_p)
df_sf <- st_as_sf(df_raster, coords = c("X", "Y"), crs = 32632)

# calcolo contributo ############
{
  intercetta <- as.numeric(mod$coefficients[1])
  
  Xp <- predict(mod, df_sf, type = "lpmatrix")
  # Then, given that matrix you can do a matrix multiplication with it and the vector of model coefficients (extracted by
  
  jd_indxs <- grep("cold_area|hot_area", colnames(Xp))
  beta <- coef(mod)
  
  b_indxs <- grep("cold_area|hot_area", names(beta))
  
  # to yield predicted values at the covariate values of x that you requested:
  acc <- Xp[,grep("cold_area|hot_area", colnames(Xp), invert = FALSE)] %*% beta[grep("cold_area|hot_area", colnames(Xp), invert = FALSE)]
  
  # and back-transform if using a non-identity link via the inverse link function. #####
  
  contr_acc <- (exp(acc)- 1)*exp(intercetta)
  contr_acc <- exp(acc)
  contr_acc_perc <- (exp(acc)- 1)/100
  
  # standard error ####
  {
    Vp <- vcov(mod)
    se <- diag(Xp %*% Vp %*% t(Xp))
    ## or more efficiently via
    # se <- rowSums(Xp %*% (Vp %*% Xp))
    se_m <- matrix(se, ncol = 109, byrow = FALSE)
    r_se_m <- rast(se_m)
    r_df_se_m <- as.data.frame(r_se_m, xy = TRUE) %>% na.omit() %>% setNames(c("x", "y", "value"))
    
    pSE <- ggplot(r_df_se_m, aes(x, y, fill = value)) + 
      geom_raster() + coord_fixed() + scale_fill_distiller(palette = "Spectral", direction = -1)
  }      
  
  df_contr <- tibble(assoluto = contr_acc,
                     percentuale = contr_acc_perc)
  
  ggplot(df_contr) + geom_boxplot(aes(y = assoluto))
  ggplot(df_contr) + geom_boxplot(aes(y = percentuale))
  
  m_perc <- matrix(df_contr$percentuale, ncol = 109,  byrow = FALSE)
  m_ass <- matrix(df_contr$assoluto, ncol = 109,  byrow = FALSE)
  
  r_ass <- terra::rast(m_ass)
  r_perc <- terra::rast(m_perc)
  
  r_df_ass <- as.data.frame(r_ass, xy = TRUE) %>% na.omit() %>% setNames(c("x", "y", "value"))
  r_df_perc <- as.data.frame(r_perc, xy = TRUE) %>% na.omit() %>% setNames(c("x", "y", "value"))
  
  p1 <- ggplot(r_df_perc, aes(x, y, fill = value)) + 
    geom_raster() + coord_fixed() + scale_fill_distiller(palette = "Spectral", direction = -1)
  
  p2 <- ggplot(r_df_ass, aes(x, y, fill = value)) + 
    geom_raster() + coord_fixed() + scale_fill_distiller(palette = "Spectral", direction = -1)
  
  cowplot::plot_grid(p1, p2, pSE, nrow = 3)
}


