# function: bestMod ####
bestMod <- function(mod.res, bic = TRUE) {
  log_print( sprintf("modelli tra cui scegliere: %s", length(mod.res) ), hide_notes = TRUE)
  
  AICS <- get("AICS", envir = .GlobalEnv)
  
  if(bic == TRUE) {
    ics <- map(mod.res, \(m) BIC({m}) )
  }else{
    ics <- map(mod.res, \(m) AIC({m}) )
  }
  
  unlist(ics) %>% which.min() -> min.ics
  
  log_print("Modello migliore: ", hide_notes = TRUE)
  log_print(mod.res[[min.ics]][["formula"]], hide_notes = TRUE)
  log_print(paste("Indice: ", min.ics), hide_notes = TRUE)
  
  
  mod.res[[min.ics]][["var.summary"]] %>% names() -> var.summary
  
  v.min <- var.summary[  !(var.summary %in% names(AICS)) ] 
  
  if(bic == TRUE) {
    return(list( BIC(mod.res[[min.ics]]), v.min))
  }else{
    return(list( AIC(mod.res[[min.ics]]), v.min))
  }
}
# bestMod(models) -> tmp

