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
  
  setwd("~/R/terni")
}

## function: buildMods ####

buildMods <- function(backward = FALSE) {
  # v_fixed <- get("v_fixed", envir = .GlobalEnv)
  AICS <- get("AICS", envir = .GlobalEnv)
  
  # v_cappa <- get("v_cappa", envir = .GlobalEnv)
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
    paste0("gam(value ~  ", x, ", gamma=1.4, family=gaussian(link=identity), data = df)"))
  
  # log_print(w %>% unlist())
  return(w)
}
# buildMods()

# function: bestMod ####
bestMod <- function(mod.res, bic = TRUE) {
  log_print( sprintf("modelli tra cui scegliere: %s", length(mod.res) ), hide_notes = TRUE)
  
  if(bic == TRUE) {
    ics <- map(mod.res, \(m) BIC({m}) )
    
  }else{
    ics <- map(mod.res, \(m) AIC({m}) )
  }
  
  ics %>% which.min() %>% as.numeric() -> min.ics
  
  log_print("Modello migliore: ", hide_notes = TRUE)
  log_print(mod.res[[min.ics]][["formula"]], hide_notes = TRUE)
  log_print(paste("Indice: ", min.ics), hide_notes = TRUE)
  
  mod.res[[min.ics]][["var.summary"]] %>% names() -> v.min
  
  if(bic == TRUE) {
    return(list( BIC(mod.res[[min.ics]]), v.min, ics))
  }else{
    return(list( AIC(mod.res[[min.ics]]), v.min, ics))
  }
}
# bestMod(models) -> tmp


## function: sceltaVar ####

sceltaVar <- function() {
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
  log_print(unlist(aicVar[[3]]), hide_notes = TRUE)
  
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
      
      q <- cor(df %>% dplyr::select(all_of(backwardVars)), use = "pairwise.complete.obs") %>% 
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
        ## WARNING: qui dovremmo verificare se in AICS le variabili sono ancora correlate 
        
        sceltaVar()
      }
    }
    
    # se siamo qui...
    log_print("Il backward non è migliore. Verifica della correlazione per mod(N)", hide_notes = TRUE)
    log_print(sprintf("Modell N: %s", paste(names(AICS), collapse = " - " ) ), hide_notes = TRUE)
    
    # sono qui perché il modello backward non migliora quindi controllo la correlazione del modello N
    q <- cor(df %>% dplyr::select(names(AICS)), use = "pairwise.complete.obs") %>% data.frame()
    
    ## INFO metodo alternativo per la correlazione ####
    # df %>% dplyr::select( names(AICS) ) %>% correlation(redundant = FALSE) 
    
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
    return(NA)
  }
  
  
  assign("v_variabili", v_variabili[!v_variabili %in% c(names(AICS), v_dead)], envir = .GlobalEnv)
  
  log_print("Fine per scelta MODELLO", hide_notes = TRUE)
  log_print(paste(names(AICS), collapse = " + "), hide_notes = TRUE)
  stop("====")
}


## Test A -> Z #####
{
  pltnt <- "Cr_i"
  df <- read_csv(glue::glue("data/dataframes/df_finale_{pltnt}.csv"), show_col_types = FALSE)
}

v_meteo <- grep("mean", names(df), value = FALSE) # le variabili meteo (media)
v_buf200 <- grep("200", names(df), value = TRUE)[1:4] # solo i buffer 200
v_urban_atlas <- grep("s8_sup", names(df), value = TRUE)
v_acciaieria <- c("cold_area", "hot_area", "scrapyard")

# v_variabili <- names(df)[c(vm, 12, 103:112)] # meteo mean e spaziali
# v_variabili <- names(df)[c(vm)] # solo le meteo
# v_variabili <- names(df)[c(103:166)] # 
# v_variabili <- names(df)[c(103:112)] # solo le spaziali
df_terni_mensili_correlazione <- read_excel("data/df_terni_mensili_correlazione.xlsx", 
                                            sheet = " Variabili scelte")
v_scelte <- df_terni_mensili_correlazione$`Variabili scelte`

# v_variabili <- v_scelte
# v_variabili <- c("t2m_mean", "t2m_IQR", "tmin2m_IQR", "tmax2m_IQR", "tp_max", "rh_IQR" )
# v_variabili <- c("t2m_mean", "t2m_IQR", "tmin2m_IQR", "tmax2m_IQR", "tp_max", "rh_IQR", "u10m_IQR" )
# v_variabili <- c("t2m_mean", "t2m_IQR", "tmin2m_IQR", "tmax2m_IQR", "tp_max", "rh_IQR", "u10m_IQR", v_buf200 )
v_variabili <- c("t2m_mean", "t2m_IQR", "tmin2m_IQR", "tmax2m_IQR", "tp_max", "rh_IQR", "u10m_IQR", v_buf200, v_acciaieria)


# df[v_variabili] %>% View()
# is.na(df[v_variabili])
# df[v_variabili] %>% as.matrix() %>% cor() %>% corrplot::corrplot()

# variabili di ambiente ####
assign("v_variabili", v_variabili, envir = .GlobalEnv)
assign("AICS", list(), envir = .GlobalEnv)
assign("v_dead", c(), envir = .GlobalEnv)
assign("N", 0, envir = .GlobalEnv)

fn <- file.path("stazione.log")
lf <- log_open(fn)

# funzione ricorsiva ####
sceltaVar()

log_close()