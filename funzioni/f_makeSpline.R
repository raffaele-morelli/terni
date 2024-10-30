makeSpline <- function(v) {
  
  return(
    lapply(v, function(x) {
      unlist(x) %>% unique() -> i
      
      v_lineari <- c('s8_sup_200', 's6_sup_200', 
                     'imp_200', 'bh_200', 'pop_200', 'ml_200', 
                     'm_dis_ferr', 'min_d')
      
      case_when(
        i %in% v_lineari ~ paste0("s(", x, ", k = 1)"),
        i %in% c("cold_area", "hot_area") ~ paste0("s(", x, ", k = 6)"),
        .default = paste0("s(", x, ")")
      )
      
    })
  )
}
