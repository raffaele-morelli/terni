## init ####
{
  library(readr)
  library(dplyr)
  library(logr)
  library(purrr)
  # library(mgcv)
  library(stringr)
  # library(correlation)
  # library(readxl)
  # library(glue)
  
  setwd("~/R/terni")

  df <- read_csv(glue::glue("data/dataframes/df_finale_lod.csv"), show_col_types = FALSE)
  
  df %>% mutate(
    TOT_CR = Biomass_Burning_CR + Soil_Dust_CR + Steel_Plant_CR + Road_Dust_CR + Brake_Dust_CR,
    TOT_NCR = Biomass_Burning_NCR + Soil_Dust_NCR + Steel_Plant_NCR + Road_Dust_NCR + Brake_Dust_NCR
  ) -> df
  
}

inner <- list()
for (s in df$site) {
  map(names(df)[92:247], \(var) {
    filter(df, site != s) %>% select(var) %>% unique() %>% nrow()
  }) -> inner[[s]]
  names(inner[[s]]) <- names(df)[92:247]
}

saveRDS(inner, "minimi.RDS")

map(minimi, \(x) { 
  unlist(x)
}) -> Z

do.call(rbind, Z) -> X

apply(X, 2, min) %>% as.list() -> kappas
names(kappas) <- names(df)[92:247]
saveRDS(kappas, "kappas.RDS")
