
## function: buildMods ####

buildMods <- function(backward = FALSE) {
  # v_fixed <- get("v_fixed", envir = .GlobalEnv)
  AICS <- get("AICS", envir = .GlobalEnv)
  v_variabili <- get("v_variabili", envir = .GlobalEnv)
  v_dead <- get("v_dead", envir = .GlobalEnv)
  kappas <- get("kappas", envir = .GlobalEnv)
  family <- get("family", envir = .GlobalEnv)
  
  
  # costruisce le "spline testuali"
  source("~/R/terni/funzioni/f_makeSpline.R")

  
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
  w <- lapply(z[,  ncol(z)], function(x) {
    # paste0("gam(value ~  ", x, ", gamma=1.4, family={family}, data = df)")
    glue("gam(value ~  {x}, gamma=1.4, family={family}, data = df)")
  })
  
  # log_print(w %>% unlist())
  return(w)
}
# buildMods()
