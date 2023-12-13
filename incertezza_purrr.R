args <- commandArgs(trailingOnly = TRUE)
cat(args, sep = "\n")

# pltnt <- "PM10" # args[1] #### SET inquinante ####

# cat("############# ", pltnt, "\n")

## init ####
{
  library(readr)
  library(dplyr)
  library(logr)
  library(purrr)
  library(mgcv)
  library(stringr)
  library(correlation)
  library(readxl)
  library(glue)
  library(FSMUMI)
  library(modelr)
  
  setwd("~/R/terni")
  
  modelli <- readRDS(glue("~/R/terni/rds_all/modelli_all_clean.RDS"))
  traccianti <- readRDS("~/R/terni/traccianti.RDS")
}

big_list <- list()

for (pltnt in traccianti) {
  df <- read_csv(glue::glue("data/dataframes/df_finale_lod_clean.csv"), show_col_types = FALSE)
  
  index <- grep(pltnt, names(df))
  names(df)[index] <- "value"
  
  map(seq(1:100), \(r) {
    cat("pltnt: ", pltnt, " : ", r, "\n")
    my_list <- list()
    
    # blocco calcolo indici ####
    tdf <- slice_sample(df, prop = 0.8) # training
    pdf <- anti_join(df, tdf, by = c("site", "data", "data_inizio", "data_fine")) # predict
    
    # addestriamo il modello sul DF di training
    gam_tdf <- gam(formula(modelli[[pltnt]]), data = tdf, family = family(modelli[[pltnt]]))
    
    # applichiamo al DF di predict
    gam_pdf <- predict.gam(gam_tdf, newdata = pdf)
    
    # Factor of 2 ####
    {  
      tmpdf <- cbind(exp(as.numeric(gam_pdf)), pdf$value) %>% 
        as.data.frame() %>%  
        setNames(c("pred", "obs"))
      
      n <- length(pdf$value)
      tmpdf %>% 
        mutate(flag = ifelse(between(pred/obs, 0.5, 2), pred/obs, 0) ) %>% 
        filter(flag > 0) %>% 
        nrow() -> nvalidate
    }
    
    # Fractional BIAS  ####
    compute.fb(tmpdf$pred, tmpdf$obs) -> fb
    
    # Normalized Mean Square Error  ####
    {  
      nmse <- tmpdf %>% 
        mutate(num = (obs - pred)^2, den = obs*pred) %>% 
        summarise(sum(num)/sum(den))
      
      rsq <- function (x, y) cor(x, y) ^ 2
      
      rsq(pdf$value, as.numeric(gam_pdf))
      rsq(gam_tdf$y, gam_tdf$fitted.values)
    }
    
    
    c(compute.rmse(pdf$value, exp(as.numeric(gam_pdf))),  # 20%
      compute.rmse(gam_tdf$y, gam_tdf$fitted.values), # 80%
      rsq(pdf$value, exp(as.numeric(gam_pdf))), # 20%
      rsq(gam_tdf$y, gam_tdf$fitted.values), # 80%
      nvalidate/n, # FAC2
      fb, # Fractional BIAS
      nmse # Normalized Mean Square Error
    )
    
  }) -> my_list
  
  
  saveRDS(my_list, glue("~/R/terni/incertezza/incertezza_{pltnt}.RDS"))
  # incertezza <- readRDS("~/R/terni/incertezza/incertezza.RDS")
  
  big_list[[pltnt]] <- do.call(rbind, my_list) %>% 
    as.data.frame() %>% 
    setNames( c("rmse20", "rmse80", "rsq20", "rsq80", "FAC2", "FB", "NMSE") )
  
}

saveRDS(big_list, file = "~/R/terni/incertezza/incertezza.RDS")

# indici CR/NCR ####


getModel <- function(vars, df) {
  source("~/R/terni/funzioni/f_makeSpline.R")
  
  ms <- makeSpline(vars) %>% paste(collapse = " + ")
  
  ms <- paste("gam(value ~ ", ms, ", gamma=1.4, family=gaussian(link=log), data = df)")
  
  mod <- eval(parse(text = ms))
  return(mod)
}


my_list <- list()
for (indice in c("TOT_CR", "TOT_NCR")) {
  
  vars <- readRDS(glue("~/R/terni/rds_indici/{indice}.rds")) 
  
  df <- read_csv(glue::glue("data/dataframes/df_finale_lod_clean.csv"), show_col_types = FALSE)
  df %>% mutate(
    TOT_CR = Biomass_Burning_CR + Soil_Dust_CR + Steel_Plant_CR + Road_Dust_CR + Brake_Dust_CR,
    TOT_NCR = Biomass_Burning_NCR + Soil_Dust_NCR + Steel_Plant_NCR + Road_Dust_NCR + Brake_Dust_NCR
  ) -> df
  
  index <- grep(indice, names(df))
  names(df)[index] <- "value"
  
  modelli <- getModel(names(vars), df)
  
  map(seq(1:100), \(r) {
    cat("indice: ", indice, " : ", r, "\n")
    my_list <- list()
    
    # blocco calcolo indici ####
    tdf <- slice_sample(df, prop = 0.8) # training
    pdf <- anti_join(df, tdf, by = c("site", "data", "data_inizio", "data_fine")) # predict
    
    # addestriamo il modello sul DF di training
    gam_tdf <- gam(formula(modelli), data = tdf, family = family(modelli))
    
    # applichiamo al DF di predict
    gam_pdf <- predict.gam(gam_tdf, newdata = pdf)
    
    # Factor of 2 ####
    {  
      tmpdf <- cbind(exp(as.numeric(gam_pdf)), pdf$value) %>% 
        as.data.frame() %>%  
        setNames(c("pred", "obs"))
      
      n <- length(pdf$value)
      tmpdf %>% 
        mutate(flag = ifelse(between(pred/obs, 0.5, 2), pred/obs, 0) ) %>% 
        filter(flag > 0) %>% 
        nrow() -> nvalidate
    }
    
    # Fractional BIAS  ####
    compute.fb(tmpdf$pred, tmpdf$obs) -> fb
    
    # Normalized Mean Square Error  ####
    {  
      nmse <- tmpdf %>% 
        mutate(num = (obs - pred)^2, den = obs*pred) %>% 
        summarise(sum(num)/sum(den))
      
      rsq <- function (x, y) cor(x, y) ^ 2
      
      rsq(pdf$value, as.numeric(gam_pdf))
      rsq(gam_tdf$y, gam_tdf$fitted.values)
    }
    
    c(compute.rmse(pdf$value, exp(as.numeric(gam_pdf))),  # 20%
      compute.rmse(gam_tdf$y, gam_tdf$fitted.values), # 80%
      rsq(pdf$value, exp(as.numeric(gam_pdf))), # 20%
      rsq(gam_tdf$y, gam_tdf$fitted.values), # 80%
      nvalidate/n, # FAC2
      fb, # Fractional BIAS
      nmse # Normalized Mean Square Error
    )
    
  }) -> my_list

  saveRDS(my_list, glue("~/R/terni/incertezza/incertezza_{indice}.RDS"))
}
