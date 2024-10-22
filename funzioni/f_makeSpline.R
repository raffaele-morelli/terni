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
                     'm_dis_ferr', 'min_d')

      case_when(
        i %in% v_lineari ~ paste0("s(", x, ", k=1)"),
        i %in% c("cold_area", "hot_area") ~ paste0("s(", x, ", k = 4)"),
        .default = paste0("s(", x, ")")
      )

    })
  )
}
