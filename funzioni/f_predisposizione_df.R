# init ####
{
  library(sf)
  library(dplyr)
  library(readr)
  library(stringr)
  library(stringi)
  library(readxl)
  library(glue)
  library(reshape2)

  pt_misura <- st_read("~/R/terni/data/shp/punti_misura.shp")
  
  sites <- pt_misura$Site
  dists <- c(25, 50, 75, 100, 200) # i buffer da considerare
  outdir <- "~/R/terni/data/dataframes"
  
} 

test <- list()

estrai <- function(var, site) {
  print(var)
  
  fls <- list.files(outdir, pattern = glue("^df_{var}"), full.names = TRUE)
  
  app <- list()
  lapply(fls, function(f) {
    read_csv(f, show_col_types = FALSE) %>% 
      melt(id.vars = 'pt_misura$Site') %>% 
      filter(`pt_misura$Site` == site) %>%
      dplyr::select(value) %>% setNames(c("value"))
  }) -> app
  
  c_names <- stri_paste(dists)
  
  df <- do.call(cbind, app) %>% setNames(c_names)
  print(df)
  test[["var"]] <- df
}
# estrai("kndvi", "PI") %>% as.matrix() %>% cor() %>% corrplot()


siti <- pt_misura$Site

appoggio <<- list()
for (s in siti) {
  appoggio[[s]] <-  estrai("kndvi", s)
} 


# indici di vegetazione e area fogliare ####
do.call(cbind, appoggio) -> df_indici

indx <- grepl("200|100", names(df_indici)) # prendiamo solo i buffer a 200m
df_indici <- df_indici[, c(indx)]

new_ass <- seq(as.Date("2016-11-01"), by = "month", length.out = 15) %>% 
  as.data.frame() %>% 
  setNames(c("data"))

df_indici <- cbind(new_ass, df_indici)

melt(df_indici, id.vars = "data") -> df_indici_m

# associazione inquinanti ####
period_mese <- read_delim("data/period_mese.csv", 
                          delim = ";", escape_double = FALSE, 
                          col_types = cols(data_inizio = col_date(format = "%d/%m/%Y"), 
                                           data_fine = col_date(format = "%d/%m/%Y")), 
                          trim_ws = TRUE)

terni_pltnt <- read_excel("data/TERNI PM Elements Data_rev2.xlsx")
terni_pltnt$data_inizio <- as.Date( terni_pltnt$data_inizio )
terni_pltnt$data_fine <- as.Date( terni_pltnt$data_fine )

names(terni_pltnt)[3] <- "site"

df_pltnt <- inner_join(period_mese, terni_pltnt, by = c("data_inizio", "data_fine") ) 

df_indici_m$variable <- gsub(".200", "", df_indici_m$variable)
names(df_indici_m)[2] <- "site"
names(df_indici_m)[3] <- "kndvi"

left_join(
  df_pltnt, 
  df_indici_m, by = c("data", "site")
) -> df

# variabili meteo ####

df_terni_meteo_mensili <- read_csv("data/dataframes/df_terni_meteo_mensili_periodo.csv", show_col_types = FALSE) %>% arrange(data)
v_meteo <- names(df_terni_meteo_mensili)[5:94]

# variabili <- readxl::read_excel("data/df_terni_mensili_correlazione.xlsx", sheet = " Variabili scelte")
# v_variabili <- variabili$`Variabili scelte`
# 
# v_meteo %in% v_variabili -> indx
# indx_n <- !indx
# v_meteo[indx_n] -> vt

# dplyr::select(df_terni_meteo_mensili, -c(vt) ) -> df_terni_meteo_mensili_A
df_terni_meteo_mensili_A <- df_terni_meteo_mensili
# identical(df_terni_meteo_mensili_A, df_terni_meteo_mensili_B)

dplyr::inner_join(df, df_terni_meteo_mensili_A) -> df_meteo

# imperviousness ####
df_imperviousness <- read_csv("data/dataframes/df_imperviousness.csv", show_col_types = FALSE)

names(df_imperviousness)[1:5] <- paste("imp", colnames(df_imperviousness)[1:5], sep = "_")

df_imp <- inner_join(df_meteo, df_imperviousness, by = "site")


# building_heights ####
df_building_heights <- read_csv("data/dataframes/df_building_heights.csv", show_col_types = FALSE)
df_building_heights[is.na(df_building_heights)] <- 0

names(df_building_heights)[1:5] <- paste("bh", colnames(df_building_heights)[1:5], sep = "_")

df_bh <- inner_join(df_imp, df_building_heights, by = "site")

# popolazione_residente ####
df_popolazione_residente <- read_csv("data/dataframes/df_popolazione_residente.csv", show_col_types = FALSE)
names(df_popolazione_residente)[1:5] <- paste("pop", colnames(df_popolazione_residente)[1:5], sep = "_")

df_pop <- inner_join(df_bh, df_popolazione_residente, by = "site")

# strade ####
df_strade_min_dist <- read_csv("data/dataframes/df_strade_min_dist.csv", show_col_types = FALSE)
names(df_strade_min_dist)[1] <- "min_d"

df_min_d <- inner_join(df_pop, df_strade_min_dist, by = "site")

# lunghezza strade ####
df_strade_ml <- read_csv("data/dataframes/df_strade_ml.csv", show_col_types = FALSE)

df_strade_ml[is.na(df_strade_ml)] <- 0

names(df_strade_ml)[1:5] <- paste("ml", colnames(df_strade_ml)[1:5], sep = "_")

df_ml <- inner_join(df_min_d, df_strade_ml, by = "site")

# acciaieria ####
df_acc_dist <- read_csv("data/dataframes/df_acc_dist.csv", show_col_types = FALSE)
df_acc <- inner_join(df_ml, df_acc_dist, by = "site")

# urban atlas ###
codes_2018 <- c(11100, 11210, 11220, 11230, 11240, 12100, 12210, 12220)
cod_str <- c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8")

df_urban_atlas <- read_csv("data/dataframes/df_urban_atlas.csv", show_col_types = FALSE)
df_urban_atlas[is.na(df_urban_atlas)] <- 0

names(df_urban_atlas)[1:5] <- gsub("_area", "", names(df_urban_atlas)[1:5])
names(df_urban_atlas)[1:5] <- paste("sup", colnames(df_urban_atlas)[1:5], sep = "_")

lapply(codes_2018, function(x) {
  inx <- grepl(x, codes_2018)
  filter(df_urban_atlas, var == x) -> df_tmp
  
  names(df_tmp)[1:5] <- paste(cod_str[inx], colnames(df_tmp)[1:5], sep = "_")
  
  return( dplyr::select(df_tmp, -c(var, site)) )
}) -> pippo

do.call(cbind, pippo) %>% 
  cbind(
    df_urban_atlas$site %>% unique() %>% as.data.frame() %>% setNames("site") 
  ) -> df_sup

inner_join(df_acc, df_sup, by = "site") -> df_acc


# ferrovia ####
df_ferrovia_min_dist <- read_csv("data/dataframes/df_ferrovia_min_dist.csv", show_col_types = FALSE)
names(df_ferrovia_min_dist)[1] <- "m_dis_ferr"
inner_join(df_acc, df_ferrovia_min_dist, by = "site") -> df_finale

outdir <- "~/R/terni/data/dataframes"

df_finale %>% mutate(
  TOT_CR = Biomass_Burning_CR + Soil_Dust_CR + Steel_Plant_CR + Road_Dust_CR + Brake_Dust_CR,
  TOT_NCR = Biomass_Burning_NCR + Soil_Dust_NCR + Steel_Plant_NCR + Road_Dust_NCR + Brake_Dust_NCR
) -> df_finale

# write_csv(df, "~/R/terni/data/dataframes/df_finale_raw.csv")
write_csv(df_finale, file = glue::glue("{outdir}/df_finale_raw.csv") )

# limiti e cambio unità di misura ####
df_finale_raw <- read_csv(glue::glue("{outdir}/df_finale_raw.csv"), show_col_types = FALSE)
limiti <- read_excel("~/R/terni/data/limiti.xlsx")

# tmp <- df_finale

# for (l in names(limiti)) {
#   tmp[[l]] <- ifelse(tmp[[l]] < as.numeric(limiti[l]), 
#                      NA, 
#                      tmp[[l]]
#   )
# }

# write_csv(tmp, file = glue::glue("{outdir}/df_finale_raw_lod.csv") )


# standardizzazione ####
# df_std <- tmp[, 92:249] %>% scale() %>% as.data.frame()
# df <- cbind(tmp[, 1:91], df_std)

# df[is.na(df)] <- 0
# names(df)[11] <- "value"

# write_csv(df, file = glue::glue("{outdir}/df_finale_raw_lod_std.csv"))

# non_zero <- function(x) {
#   return( ifelse(x == 0, 1/1000000000000, x))
# }

# tmp %>%
#   mutate(across(all_of(names(tmp[11:20])), non_zero )) %>% 
#   write_csv(file = glue::glue("{outdir}/df_finale_lod.csv"))
# 
# write_csv(limiti, file = glue::glue("{outdir}/limiti.csv"))
# 
# # verifica dei limiti del nuovo df ####
# 
# limiti <- read_csv("data/dataframes/limiti.csv")
# df_finale <- read_csv("data/dataframes/df_finale_lod.csv", show_col_types = FALSE)
# 
# df_finale[names(limiti)] %>%
#   reshape2::melt() %>%
#   group_by(variable) %>%
#   summarise(m = min(value)) %>% View()
# 
# df_finale[names(tmp[11:20])] %>%
#   reshape2::melt() %>%
#   group_by(variable) %>%
#   filter(value == 0) %>% 
#   summarise(m = n()) %>% View()
