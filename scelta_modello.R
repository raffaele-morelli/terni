## init ####
{
  library(readr)
  library(dplyr)
  library(logr)
  library(purrr)
  library(mgcv)
  library(stringr)
  library(correlation)
  
  library(datiInquinanti)
  library(datiMeteo)
  
  setwd("~/R/pulvirus_reloaded")
}

## function: buildMods ####

buildMods <- function(backward = FALSE) {
  # v_fixed <- get("v_fixed", envir = .GlobalEnv)
  AICS <- get("AICS", envir = .GlobalEnv)
  
  v_cappa <- get("v_cappa", envir = .GlobalEnv)
  v_variabili <- get("v_variabili", envir = .GlobalEnv)
  v_dead <- get("v_dead", envir = .GlobalEnv)
  
  # costruisce le "spline testuali"
  makeSpline <- function(v) {
    return(lapply(v, function(x) paste0("s(", x, ")") ))
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
    paste0("gam(log(value) ~ weekday + lock + s(mese, bs='cc', k=12) + s(jd, k=", v_cappa, ") + ", x, ", data = df)"))
  
  # log_print(w %>% unlist())
  return(w)
}


# function: bestMod ####
bestMod <- function(mod.res) {
  aics <- map(mod.res, function(m) AIC({m}) )
  aics %>% which.min() %>% as.numeric() -> min.aic
  
  log_print("Modello migliore: ", hide_notes = TRUE)
  log_print(mod.res[[min.aic]][["formula"]], hide_notes = TRUE)
  log_print(paste("Indice: ", min.aic), hide_notes = TRUE)
  
  mod.res[[min.aic]][["var.summary"]] %>% names() -> v.min
  
  return( list( AIC(mod.res[[min.aic]]), v.min[-c(1:4)], aics) )
}


## function: sceltaVar ####

sceltaVar <- function(cappa) {
  AICS <- get("AICS", envir = .GlobalEnv)
  v_variabili <- get("v_variabili", envir = .GlobalEnv)
  v_dead <- get("v_dead", envir = .GlobalEnv)
  
  w <- buildMods() # costruisce le stringhe dei modelli
  if(is.null(w)) {
    return("Nessun modello da costruire")
  }
  
  log_print(sprintf("---------- START: %s", N), hide_notes = TRUE)
  
  # conterrà gli oggetti GAM calcolati sulle stazioni
  models <- map(w, \(y) eval(parse(text = y)))
  
  aicVar <- bestMod(models) # AIC del modello migliore
  
  log_print(unlist(w), hide_notes = TRUE)
  log_print(unlist(aicVar[[3]]) , hide_notes = TRUE)
  
  # una lista di appoggio da concatenare in AICS
  tmp <- list()
  tmp[[last(aicVar[[2]])]] <- c(tmp, aicVar[[1]])
  
  # Log delle risultanze
  log_print(sprintf("Variabile scelta: %s - AIC %s ", last(aicVar[[2]]), round( as.numeric(aicVar[1]), 2)), hide_notes = TRUE )
  
  # Salvataggio lista con gli AIC dei modelli elaborati finora
  AICS <- c(AICS, tmp)
  assign("AICS", AICS, envir = .GlobalEnv)
  
  N <- length(AICS)
  assign("N", N, envir = .GlobalEnv)
  
  if( N < 2) {
    sceltaVar()
  }
  
  if( AICS[[N]][[1]] < AICS[[N-1]][[1]] ) {
    log_print("Il modello N è migliore del precedente, verifica del backward", hide_notes = TRUE)
    
    w <- buildMods(backward = TRUE)
    
    if(is.null(w)) {
      log_print("FINE VARIABILI")
      assign("v_variabili", v_variabili[!v_variabili %in% c(names(AICS), v_dead) ], envir = .GlobalEnv)
      
      return(NA)
    }
    models <- map(w, \(y) eval(parse(text = y)))
    
    aicBack <- bestMod(models)  # AIC del backward
    log_print(sprintf("Dati backward:  %s - %s", last(aicBack[[2]]), aicBack[[1]] ), hide_notes = TRUE)
    
    # log_print(unlist(w), hide_notes = TRUE)
    # log_print(t(aicBack[[3]]), hide_notes = TRUE)
    log_print( cbind(unlist(w), unlist(aicBack[[3]])), hide_notes = TRUE )
    
    if(is.null(aicBack)) {
      return(NA)
    }
    
    if( aicBack[[1]] < AICS[[N]][[1]] ) {
      n_1 <- names(AICS)[-c(length(names(AICS))-1 )]
      
      backwardVars <- c(n_1, last(aicBack[[2]]))
      
      q <- cor(df %>% select(all_of(backwardVars)), use = "pairwise.complete.obs") %>% 
        data.frame()
      
      # check di correlazione sulle variabili del backward
      if( all(abs(q[last(aicBack[[2]]), 1:(ncol(q)-1)]) < 0.7) ) {
        
        AICS <- AICS[-c(length(AICS)-1)]
        AICS[[ last(aicBack[[2]]) ]] <- list(aicBack[[1]])
        
        assign("v_variabili", v_variabili[!v_variabili %in% c(names(AICS), v_dead) ], envir = .GlobalEnv)
        
        log_print("Scelgo il BACKWARD", hide_notes = TRUE)
        sceltaVar()
      }else{
        # metto la variabile nelle dead perché troppo correlata
        log_print(sprintf("Variabile altamente correlata: %s ", last(aicBack[[2]]) ), hide_notes = TRUE)
        log_print(q[last(aicBack[[2]]), 1:(ncol(q)-1)] )
        log_print("Scelgo il modello N ", hide_notes = TRUE)
        
        assign("v_dead", c(v_dead, last(aicBack[[2]] )), envir = .GlobalEnv)
        assign("v_variabili", v_variabili[!v_variabili %in% c(names(AICS), v_dead) ], envir = .GlobalEnv)
        ## WARNING: qui dovremmo verificare se in AICS le variabili sono ancora correlate ####
        
        sceltaVar()
      }
    }
    
    # se siamo qui...
    log_print("Il backward non è migliore. Verifica della correlazione per mod(N)", hide_notes = TRUE)
    log_print(sprintf("Modell N: %s", paste(names(AICS), collapse = " - " ) ), hide_notes = TRUE)
    
    # sono qui perché il modello backward non migliora quindi controllo la correlazione del modello N
    q <- cor(df %>% select(names(AICS)), use = "pairwise.complete.obs") %>% data.frame()
    
    ## INFO metodo alternativo per la correlazione ####
    # df %>% select( names(AICS) ) %>% correlation(redundant = FALSE) 
    
    if( all(abs(q[last(names(AICS)), 1:(ncol(q)-1)]) < 0.7) ) {
      log_print("Scelgo il modello N", hide_notes = TRUE)
      
      # tolgo dalla lista delle variabili quelle del modello scelto visto che non sono correlate
      assign("v_variabili", v_variabili[!v_variabili %in% c(names(AICS), v_dead)], envir = .GlobalEnv)
      sceltaVar()
      # return("the end")
      
    }else{
      # devo eliminare la variabile perché troppo correlata
      log_print("Variabile correlata > 0.7", hide_notes = TRUE)
      log_print(q[last(names(q)), 1:(ncol(q)-1)], hide_notes = TRUE)
      
      assign("v_dead", c(v_dead, c(last(names(AICS))) ), envir = .GlobalEnv )
      assign("v_variabili", v_variabili[!v_variabili %in% c(last(names(AICS)), v_dead) ], envir = .GlobalEnv)
      
      AICS[[last(names(AICS))]] <- NULL
      assign("AICS", AICS, envir = .GlobalEnv)
      
      log_print("Scelgo il modello N-1", hide_notes = TRUE)
      sceltaVar()
      # return("the end")
    }
  }else{
    log_print("Fine per scelta MODELLO iniziale", hide_notes = TRUE)
    # qui devo togliere l'ultima variabile
    # assign("v_dead", c(v_dead, c(names(AICS[N])) ), envir = .GlobalEnv)
    AICS[[last(names(AICS))]] <- NULL
    
    assign("AICS", AICS, envir = .GlobalEnv)
    assign("v_variabili", v_variabili[!v_variabili %in% c(names(AICS), v_dead)], envir = .GlobalEnv)
    stop("=>>> the end")
  }
  
  
  assign("v_variabili", v_variabili[!v_variabili %in% c(names(AICS), v_dead)], envir = .GlobalEnv)
  
  log_print("Fine per scelta MODELLO", hide_notes = TRUE)
  log_print(paste(names(AICS), collapse = " + "), hide_notes = TRUE)
  stop("====")
}

## Test A -> Z #####


eu_code <- "IT1011A"
pltnt <- "pm10"

# dati sull'inquinante ####
TERNI_PM_Elements <- read_excel("data/TERNI PM Elements Data_rev.xlsx")


# variabili di ambiente ####
assign("v_variabili", v_variabili, envir = .GlobalEnv)
assign("v_cappa", length(unique(df$reporting_year)))

assign("AICS", list(), envir = .GlobalEnv)
assign("v_dead", c(), envir = .GlobalEnv)
assign("N", 0, envir = .GlobalEnv)

fn <- file.path("stazione.log")
lf <- log_open(fn)

# funzione ricorsiva ####
sceltaVar()

log_close()