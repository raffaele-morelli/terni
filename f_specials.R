# df <- df %>% dplyr::mutate(value = ifelse(value <= 0.0099500760, NA, value) )

df <- readr::read_csv("~/R/terni/data/dataframes/df_finale_raw.csv", show_col_types = FALSE)
traccianti <- readRDS("~/R/terni/rds_out/traccianti.RDS") 
traccianti <- traccianti[!(traccianti %in% c("TOT_CR", "TOT_NCR"))]

map(traccianti, \(t) {
  list(t, DescTools(df[[t]], na.rm = TRUE), as.numeric(limiti[[t]])  )
  # as.numeric(limiti[[t]])
}) -> anomalie

do.call(rbind, anomalie) %>% as.data.frame() %>% setNames(c("t", "moda", "lod"))