makeSpline <- function(v) {
  # kappas <- get("kappas", envir = .GlobalEnv)
  # v_spaziali <- get("v_spaziali", envir = .GlobalEnv)
  
  return(
    lapply(v, function(x) {
      unlist(x) %>% unique() -> i
      
      # k <- filter(kappas, var == i) %>% select(kappas) %>% as.numeric()
      # log_print(sprintf("var %s kappa %s", i, k))
      v_lineari <- c('s8_sup_200', 's6_sup_200', 
                     'imp_200', 'bh_200', 'pop_200', 'ml_200', 
                     'cold_area', 'hot_area', 'scrapyard', 'm_dis_ferr', 'min_d')
      
      # v_spat <- readRDS("~/R/terni/rds_out/v_spaziali.RDS")
      
      if(i %in%  v_lineari) {
        # paste0("s(", x, ", k=1)")
        paste0(" ", x, " ")
      }else{
        paste0("s(", x, ")")
      }
      
      # case_when(
      #   k > 9 ~  paste0("s(", x, ", k=5)"),
      #   k <= 10 & k > 5  ~  paste0("s(", x, ", k=", k-2, ")"),
      #   .default = paste0("s(", x, ", k=1)")
      # )
      # paste0("s(", x, ", k=4)")
      # paste0("s(", x, ")")
    })
  )
}
