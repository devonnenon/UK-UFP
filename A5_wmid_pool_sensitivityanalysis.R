# -------------
# Pool West Midlands sites
# -------------

mainlist[["wmid_pool"]] <- lapply(outcomes, function(outcome){
  
  # Extract coefficients and variances from west midlands models 
  coefs <- unlist(lapply(birmsites, function(site) 
    mainlist[[site]][[outcome]][["modmain"]][["coefficients"]][["ufp01"]]))
  
  variances <- unlist(lapply(birmsites, function(site) 
    vcov(mainlist[[site]][[outcome]][["modmain"]])["ufp01","ufp01"]))
  
  # Standard meta analysis with fixed effects
  meta <- mixmeta(coefs ~ 1, variances, method = "fixed")
  
  # Predict change for 10,000 unit increase of UFP
  est <- as.data.frame(t(exp(predict(meta, ci = T)[1,]*10000)))
  colnames(est) <- c("est", "lower", "upper")
  
  # Calculate as % change
  perc <- (est - 1)*100
  
  results <- list(
    modmain = meta,
    estRR = est,
    estperc = perc,
    outcome = outcome
  )
  return(results)
}) ; names(mainlist[["wmid_pool"]]) <- outcomes