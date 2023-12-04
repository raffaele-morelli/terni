# args <- commandArgs(trailingOnly = TRUE)
# cat(args, sep = "\n")

# pltnt <- "Cr_i" # args[1] #### SET inquinante ####
# dir <- args[2] ### SET directory ####

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
  
  modelli <- readRDS(glue("~/R/terni/rds_all/modelli_all.RDS"))
  # frml <- formula(modelli[[pltnt]])
  # set.seed(1974)
  
}

my_list <- list()

for (pltnt in names(modelli)) {
  # if(pltnt %in% c("Al_s", "As_s", "B_i") ) {
  #   next
  # }
  
  fn <- file.path(glue("log/cross_valid_{pltnt}.log"))
  lf <- log_open(fn)
  
  # va riletto ogni volta altrimenti "value" viene trovato più volte
  df <- read_csv(glue::glue("data/dataframes/df_finale_lod.csv"), show_col_types = FALSE)

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

saveRDS(my_list, file = "~/R/terni/cross_validation.RDS")
# colnames(my_df) <- c("rmse20", "rmse80", "rsq20", "rsq80", "FAC2", "FB", "NMSE")

# arrotonda <- function(x) {
#   unlist(x) %>% as.vector -> y
#   return( round(y, 4) )
# }

# my_df %>% 
#   mutate(
#     across( all_of(names(my_df)), arrotonda )
#     ) -> my_df

# tibble::rownames_to_column(my_df) -> my_df

# write_csv(my_df, "~/R/terni/data/dataframes/df_cross_validazione.csv")
