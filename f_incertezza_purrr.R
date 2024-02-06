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
  
  met <- "test8"
  modelli <- readRDS(glue("~/R/terni/rds_gaussian_{met}/modelli_{met}_clean.RDS"))
  traccianti <- readRDS("~/R/terni/rds_out/traccianti.RDS")
  
  blacklist_inquinanti <- read_csv("data/blacklist_inquinanti.csv", show_col_types = FALSE)
  
  traccianti <- traccianti[!(traccianti %in% blacklist_inquinanti$pltnt)]
}

big_list <- list()

set.seed(1974)

for (pltnt in traccianti) {
  df <- read_csv(glue::glue("~/R/terni/data/dataframes/df_finale_raw.csv"), show_col_types = FALSE)
  
  index <- grep(pltnt, names(df))
  names(df)[index] <- "value"
  
  map(seq(1:50), \(r) {
    cat("pltnt: ", pltnt, " : ", r, "\n")
    my_list <- list()
    
    df <- df %>% group_by(associazione_ispra)
    # is_grouped_df(df)
    
    # blocco calcolo indici ####
    tdf <- slice_sample(df, prop = 0.75) # training
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

  big_list[[pltnt]] <- do.call(rbind, my_list) %>% 
    as.data.frame() %>% 
    setNames( c("rmse20", "rmse80", "rsq20", "rsq80", "FAC2", "FB", "NMSE") )
  
}

saveRDS(big_list, file = glue("~/R/terni/rds_gaussian_{met}/incertezza.RDS"))

