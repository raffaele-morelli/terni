
## function: buildMods ####

buildMods <- function(backward = FALSE) {
  # v_fixed <- get("v_fixed", envir = .GlobalEnv)
  AICS <- get("AICS", envir = .GlobalEnv)
  v_variabili <- get("v_variabili", envir = .GlobalEnv)
  v_dead <- get("v_dead", envir = .GlobalEnv)
  # cappas <- get("cappas", envir = .GlobalEnv)
  
  
  # costruisce le "spline testuali"
  makeSpline <- function(v) {
    return(
      lapply(v, function(x) {
        unlist(x) %>% unique() -> i
        # paste0("s(", x, ", k=", cappas[[i]], ")")      })
        case_when(
          x == "tp_median" ~ paste0("s(", x, ", k=3)"),
          x == "u10m_min" ~ paste0("s(", x, ", k=5)"),
          x == "u10m_max" ~ paste0("s(", x, ", k=8)"),
          x == "v10m_median" ~ paste0("s(", x, ", k=9)"),
          x == "wspeed_min" ~ paste0("s(", x, ", k=8)"),
          x == "pblmin_median" ~ paste0("s(", x, ", k=3)"),
          x == "pbl00_median" ~ paste0("s(", x, ", k=6)"),
          x == "pblmin_IQR" ~ paste0("s(", x, ", k=9)"),
          x == "pbl00_min" ~ paste0("s(", x, ", k=1)"),
          x == "s7_sup_200" ~ paste0("s(", x, ", k=3)"),
          x == "s5_sup_200" ~ paste0("s(", x, ", k=9)"),
          x == "s1_sup_200" ~ paste0("s(", x, ", k=7)"),
          x == "u10m_median" ~ paste0("s(", x, ", k=9)"),
          x == "pwspeed_min" ~ paste0("s(", x, ", k=7)"),
          .default = paste0("s(", x, ")")
          )
      })
    )
  }
  
  if( length(AICS) > 1 & backward == TRUE ) {
    # Si elimina la variabile (n-1), si bloccano le variabili da 1 a (n-2) e
    # quella n e poi si sceglie tra le var rimanenti quella che minimizza l'AIC
    log_print("Modelli backward: ", hide_notes = TRUE)
    # log_print(length(AICS), hide_notes = TRUE)
    # log_print(backward, hide_notes = TRUE)
    
    v_left <- v_variabili[!v_variabili %in% c(names(AICS), v_dead)]
    
    # se non ci sono più variabili usciamo
    if(length(v_left) == 0)
      return(NULL)
    
    s_left <- makeSpline(v_left)
    
    # costruisco le "spline" con le variabili in v_fixed tranne la n-1
    c_fixed <- lapply( c(names(AICS)[-c(length(AICS)-1)], v_dead), function(x) rep(x, length(v_left)) )
    
    s_fixed <- makeSpline(c_fixed)
    
    s_fixed <- do.call(cbind, s_fixed)
  }else{
    log_print("Modelli NON backward: ", hide_notes = TRUE)
    
    # le variabili non scelte (v_fixed) e non scartate (v_dead)
    v_left <- v_variabili[!v_variabili %in% c(names(AICS), v_dead)]
    
    if(length(v_left) == 0) {
      return(NULL)
    }
    
    # le combinazioni delle restanti
    s_left <- makeSpline(v_left)
    
    c_fixed <- lapply(names(AICS), function(x) rep(x, length(s_left))) 
    
    s_fixed <- makeSpline(c_fixed)
    
    s_fixed <- do.call(cbind, s_fixed)
  }
  
  x <- cbind(s_fixed, s_left)
  z <- data.frame(mod = apply(x, 1, paste0, collapse = " + ")) # per ogni riga 
  # log_print(z)
  
  # w conterrà le stringhe dei modelli
  w <- lapply(z[,  ncol(z)], function(x) 
    paste0("gam(log(value) ~  ", x, ", gamma=1.4, family=gaussian(link=log), data = df)"))
  
  # log_print(w %>% unlist())
  return(w)
}
# buildMods()
