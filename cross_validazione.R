# args <- commandArgs(trailingOnly = TRUE)
# cat(args, sep = "\n")

# dir <- args[1] ### SET directory ####

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
  
  dir <- "lineari"
  modelli <- readRDS(glue("~/R/terni/rds_{dir}/modelli_{dir}_clean.RDS"))
}

my_list <- list()

for (pltnt in names(modelli)) {
  
  fn <- file.path(glue("~/R/terni/log/cv/{dir}/cross_valid_{pltnt}.log"))
  lf <- log_open(fn)
  
  # va riletto ogni volta altrimenti "value" viene trovato più volte
  df <- read_csv(glue::glue("data/dataframes/df_finale_lod.csv"), show_col_types = FALSE)
  df %>% mutate(
    TOT_CR = Biomass_Burning_CR + Soil_Dust_CR + Steel_Plant_CR + Road_Dust_CR + Brake_Dust_CR,
    TOT_NCR = Biomass_Burning_NCR + Soil_Dust_NCR + Steel_Plant_NCR + Road_Dust_NCR + Brake_Dust_NCR
  ) -> df
  
  # sites <- unique(df$site)
  index <- grep(pltnt, names(df))
  names(df)[index] <- "value"

  for (s in unique(df$site)) {
    log_print(sprintf("Tracciante %s, site out: %s", pltnt, s), hide_notes = TRUE)
    # cat("Out", s, "\n")
    
    tdf <- filter(df, !(site %in% s)) # training
    pdf <- anti_join(df, tdf) # predict

    an.error.occured <- FALSE
    tryCatch( { 
      # addestriamo il modello sul DF di training
      gam_tdf <- gam(formula(modelli[[pltnt]]), data = tdf, family = family(modelli[[pltnt]])); 
      log_print(summary(gam_tdf), hide_notes = TRUE )
      
      # applichiamo al DF di predict
      gam_pdf <- predict.gam(gam_tdf, newdata = pdf)
      log_print(summary(gam_pdf), hide_notes = TRUE )
    }, 
    error = function(e) {
      an.error.occured <<- TRUE
      log_print(sprintf("Errors can't stop me on training: %s on %s", pltnt, s), hide_notes = TRUE )
    })
    if(an.error.occured) {
      my_list[[pltnt]][[s]] <- c(NA,  # 20%
                                 NA, # 80%
                                 NA, # 20%
                                 NA, # 80%
                                 NA, # FAC2
                                 NA, # Fractional BIAS
                                 NA # Normalized Mean Square Error
      )      
      next 
    }
    
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
    
    
    my_list[[pltnt]][[s]] <- c(compute.rmse(pdf$value, exp(as.numeric(gam_pdf))),  # 20%
                             compute.rmse(gam_tdf$y, gam_tdf$fitted.values), # 80%
                             rsq(pdf$value, exp(as.numeric(gam_pdf))), # 20%
                             rsq(gam_tdf$y, gam_tdf$fitted.values), # 80%
                             nvalidate/n, # FAC2
                             fb, # Fractional BIAS
                             nmse # Normalized Mean Square Error
    )
  }
  log_close()
}


my_mat <- do.call(rbind, my_list)
my_df <- data.frame(my_mat)

saveRDS(my_list, file = glue("~/R/terni/cross_validation_{dir}.RDS"))

