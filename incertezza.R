args <- commandArgs(trailingOnly = TRUE)
cat(args, sep = "\n")

pltnt <- args[1] #### SET inquinante ####

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
}

my_list <- list()


df <- read_csv(glue::glue("data/dataframes/df_finale_lod.csv"), show_col_types = FALSE)

index <- grep(pltnt, names(df))
names(df)[index] <- "value"

for (r in 1:30) {
  cat("RUN: ", r, "\n")
  
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
  
  
  my_list[[pltnt]][[r]] <- c(compute.rmse(pdf$value, exp(as.numeric(gam_pdf))),  # 20%
                        compute.rmse(gam_tdf$y, gam_tdf$fitted.values), # 80%
                        rsq(pdf$value, exp(as.numeric(gam_pdf))), # 20%
                        rsq(gam_tdf$y, gam_tdf$fitted.values), # 80%
                        nvalidate/n, # FAC2
                        fb, # Fractional BIAS
                        nmse # Normalized Mean Square Error
                        )
  
}



saveRDS(my_list, "~/R/terni/incertezza/incertezza.RDS")

incertezza <- readRDS("~/R/terni/incertezza/incertezza.RDS")

my_mat <- do.call(rbind, incertezza)

my_df <- data.frame(my_mat)

sapply(my_mat, function(x) { 
  as.vector(x) 
}) %>% 
  t() %>% as.data.frame() %>% 
  setNames( c("rmse20", "rmse80", "rsq20", "rsq80", "FAC2", "FB", "NMSE") ) %>% View()

# arrotonda <- function(x) {
#   unlist(x) %>% as.vector -> y
#   return( round(y, 4) )
# }
# 
# 
# my_df %>% 
#   mutate(
#     across( all_of(names(my_df)), arrotonda )
#   ) -> my_df
# 
# tibble::rownames_to_column(my_df) -> my_df
# 
# write_csv(my_df, "~/R/terni/data/dataframes/df_validazione.csv")
