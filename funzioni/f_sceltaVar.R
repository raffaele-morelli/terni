## function: sceltaVar ####

sceltaVar <- function() {
  AICS <- get("AICS", envir = .GlobalEnv)
  
  v_variabili <- get("v_variabili", envir = .GlobalEnv)
  v_dead <- get("v_dead", envir = .GlobalEnv)
  
  rds_dir <- get("rds_dir", envir = .GlobalEnv)

  w <- buildMods() # costruisce le stringhe dei modelli
  if(is.null(w)) {
    return("Nessun modello da costruire")
  }
  
  log_print(sprintf("---------- START: %s", N), hide_notes = TRUE)
  
  # conterrà gli oggetti GAM calcolati sulle stazioni
  suppressWarnings(
    models <- map(w, function(y) {
      eval(parse(text = y)) 
    })
  )

  aicVar <- bestMod(models) # AIC del modello migliore
  
  log_print(unlist(w), hide_notes = TRUE)
  # log_print(unlist(aicVar[[3]]), hide_notes = TRUE)
  
  # una lista di appoggio da concatenare in AICS
  tmp <- list()
  tmp[[last(aicVar[[2]])]] <- c(aicVar[[1]])
  
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
    log_print("Il modello N è migliore del precedente, verifica correlazione", hide_notes = TRUE)

    # verifichiamo la correlazione del modello N
    q <- cor(df %>% dplyr::select(names(AICS)), use = "pairwise.complete.obs") %>% round(3) %>% data.frame()
    
    if( all(abs(q[last(names(AICS)), 1:(ncol(q)-1)]) < 0.7) == FALSE ) {
      log_print("Non scelgo il modello N perché la variabile entrata è correlata", hide_notes = TRUE)
       
      v_dead <- c(v_dead, last(names(AICS)))
      AICS[[last(names(AICS))]] <- NULL
      
      assign("AICS", AICS, envir = .GlobalEnv)
      
      # tolgo dalla lista delle variabili quelle del modello scelto visto che non sono correlate
      assign("v_variabili", v_variabili[!v_variabili %in% c(names(AICS), v_dead)], envir = .GlobalEnv)
      sceltaVar()
      # return(NULL)
    }
    
    w <- buildMods(backward = TRUE)
    
    if(is.null(w)) {
      log_print("FINE VARIABILI")
      assign("v_variabili", v_variabili[!v_variabili %in% c(names(AICS), v_dead) ], envir = .GlobalEnv)
      
      return(NA)
    }
    suppressWarnings(
      models <- map(w, \(y) eval(parse(text = y)))
    )
    
    aicBack <- bestMod(models)  # AIC del backward
    log_print(sprintf("Dati backward:  %s - %s", last(aicBack[[2]]), aicBack[[1]] ), hide_notes = TRUE)
    
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
        assign("AICS", AICS, envir = .GlobalEnv)
        
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
    # Il backward non è migliore ed il modello N non ha variabili correlate
    # i lavori sulle variabili li ho fatti già
    log_print("Scelgo il modello N", hide_notes = TRUE)
    log_print(sprintf("Modello N: %s", paste(names(AICS), collapse = " - " ) ), hide_notes = TRUE)
    sceltaVar()
    

  }else{
    # log_print("Fine per scelta MODELLO iniziale", hide_notes = TRUE)
    # qui devo togliere l'ultima variabile
    # assign("v_dead", c(v_dead, c(names(AICS[N])) ), envir = .GlobalEnv)
    AICS[[last(names(AICS))]] <- NULL
    
    assign("AICS", AICS, envir = .GlobalEnv)
    # assign("v_variabili", v_variabili[!v_variabili %in% c(names(AICS), v_dead)], envir = .GlobalEnv)
    # 
    # saveRDS(AICS, file = glue("~/R/terni/{outdir}/{pltnt}.rds"))
    # stop("=>>> Fine per scelta MODELLO iniziale")
  }
  
  
  assign("v_variabili", v_variabili[!v_variabili %in% c(names(AICS), v_dead)], envir = .GlobalEnv)
  
  log_print("Fine per scelta MODELLO", hide_notes = TRUE)
  log_print(paste(names(AICS), collapse = " + "), hide_notes = TRUE)
  

  saveRDS(AICS, file = glue("~/R/terni/{rds_dir}/{pltnt}.rds"))
  
  stop("==== Fine per scelta MODELLO")
}