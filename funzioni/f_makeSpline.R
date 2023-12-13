makeSpline <- function(v) {
  kappas <- get("kappas", envir = .GlobalEnv)
  
  return(
    lapply(v, function(x) {
      unlist(x) %>% unique() -> i
      
      k <- filter(kappas, var == i) %>% select(kappas) %>% as.numeric()
      # log_print(sprintf("var %s kappa %s", i, k))
      
      # case_when(
      #   k > 4 ~  paste0("s(", x, ", k=5)"),
      #   k <= 4 ~  paste0("s(", x, ", k=", k-1, ")"),
      #   .default = paste0("s(", x, ", k=", k-1, ")")
      # )

      case_when(
        k > 4 ~  paste0("s(", x, ", k=5)"),
        k <= 4 ~  paste0("", x, ""),
        .default = paste0("", x, "")
      )
      
    })
  )
}