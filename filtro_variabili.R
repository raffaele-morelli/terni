## init ####
{
  library(readr)
  library(dplyr)
  library(purrr)
  library(stringr)
  
  setwd("~/R/terni")

  df <- read_csv(glue::glue("data/dataframes/df_finale_lod.csv"), show_col_types = FALSE)
  df$pblmin_IQR %>% unique() %>% length()
  
  df %>% mutate(
    TOT_CR = Biomass_Burning_CR + Soil_Dust_CR + Steel_Plant_CR + Road_Dust_CR + Brake_Dust_CR,
    TOT_NCR = Biomass_Burning_NCR + Soil_Dust_NCR + Steel_Plant_NCR + Road_Dust_NCR + Brake_Dust_NCR
  ) -> df
  
}

## Variabili #####
{
  v_meteo <- names(df)[93:182]
  
  # variabili gianluca ####
  df_terni_mensili_correlazione <- read_excel("data/df_terni_mensili_correlazione.xlsx", sheet = " Variabili scelte")
  v_scelte <- df_terni_mensili_correlazione$`Variabili scelte`
  
  v_meteo_mean <- grep("mean", names(df), value = TRUE) # le variabili meteo (media)
  v_buf200 <- grep("200", names(df), value = TRUE)[1:4] # solo i buffer 200
  v_urban_atlas <- grep("s8_sup_200|s7_sup_200|s6_sup_200|s5_sup_200|s4_sup_200|s3_sup_200|s2_sup_200|s1_sup_200", names(df), value = TRUE)
  v_acciaieria <- c("cold_area", "hot_area", "scrapyard")
  
  v_variabili <- c("kndvi", v_scelte, v_meteo, v_buf200, v_acciaieria, v_urban_atlas, "m_dis_ferr") %>% unique()
}

df <- df %>% select(c(1:10, 21:91, "TOT_CR", "TOT_NCR", v_variabili)) 

map(v_urban_atlas, \(v) {
  df %>% select(all_of(v)) %>% unique() %>% unlist() %>% length()
}) %>% set_names(v_urban_atlas) %>% as.data.frame() %>% t() -> res_v_urban_atlas

map(v_scelte, \(v) {
  df %>% select(all_of(v)) %>% unique() %>% unlist() %>% length()
}) %>% set_names(v_scelte) %>% as.data.frame() %>% t() -> res_v_scelte

map(v_meteo, \(v) {
  df %>% select(all_of(v)) %>% unique() %>% unlist() %>% length()
}) %>% set_names(v_meteo) %>% as.data.frame() %>% t() -> res_v_meteo

c(
  as.data.frame(res_v_meteo) %>% filter(V1 > 8) %>% rownames(),
  as.data.frame(res_v_scelte) %>% filter(V1 > 8) %>% rownames(),
  as.data.frame(res_v_urban_atlas) %>% filter(V1 > 16) %>% rownames()
) %>% unique() -> v_variabili_finali

df %>% 
  select(c(1:83,  v_variabili_finali)) %>% 
  write_csv(file = "data/dataframes/df_finale_lod_clean.csv")
