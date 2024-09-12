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
  library(DTWBI)
  library(glue)
  library(modelr)
  
  setwd("~/R/terni")
  
  met <- "test8"
  modelli <- readRDS(glue("~/R/terni/rds_gaussian_{met}/modelli_{met}_clean.RDS"))
}

my_list <- list()
inquinanti <- names(modelli)
# inquinanti <- inquinanti[24]

for (pltnt in inquinanti) {
  
  fn <- file.path(glue("~/R/terni/log/cv/{met}/cross_valid_{pltnt}.log"))
  lf <- log_open(fn)
  
  # va riletto ogni volta altrimenti "value" viene trovato più volte
  df <- read_csv(glue::glue("~/R/terni/data/dataframes/df_finale_raw.csv"), show_col_types = FALSE)
  
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
      gam_tdf <- gam(formula(modelli[[pltnt]]), gamma = 1.4, data = tdf, family = family(modelli[[pltnt]])); 
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
                                 NA, # adjusted r-squared for the model
                                 NA, # rsq su DF
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
    
    # check rsw vs R²
    {
      mod_orig <- gam(formula(modelli[[pltnt]]), gamma = 1.4, data = df, family = family(modelli[[pltnt]]))
    }
    
    my_list[[pltnt]][[s]] <- c(compute.rmse(pdf$value, exp(as.numeric(gam_pdf))),  # 20%
                             compute.rmse(gam_tdf$y, gam_tdf$fitted.values), # 80%
                             rsq(pdf$value, exp(as.numeric(gam_pdf))), # 20%
                             rsq(gam_tdf$y, gam_tdf$fitted.values), # 80%
                             summary.gam(mod_orig)[["r.sq"]], #adjusted r-squared for the model
                             rsq(mod_orig$y, mod_orig$fitted.values ), # rsq su modello originale
                             nvalidate/n, # FAC2
                             fb, # Fractional BIAS
                             nmse # Normalized Mean Square Error
    )
  }
  log_close()
}


my_mat <- do.call(rbind, my_list)
my_df <- data.frame(my_mat)

saveRDS(my_list, file = glue("~/R/terni/rds_gaussian_{met}/cross_validation.RDS"))

