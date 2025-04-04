################################################################################
# UFP/mortality project


# Main analysis

################################################################################

# Loop through the measurement sites
mainlist <- lapply(names(dlist), function(location){
  
  # Extract data for each site
  data <- dlist[[location]]

  # Define spline of time
  spltime <- eval(spltime_ex)
  
  # Define temperature crossbasis
  cbtemp <- eval(cbtemp_ex)
  
  # Loop on the outcomes
  causes_results <- lapply(outcomes, function(outcome){
    
    # Extract mortality data for the outcome
    mortality <- data[[outcome]]

    # Run main model
    modmain <- glm(mortality ~ ufp01 + spltime + dow + holiday + cbtemp , 
                   data, family=quasipoisson)
    
    # Extract RR and 95% CI for X unit increase (defined in params)
    est <- ci.exp(modmain, subset="ufp01", ctr.mat=matrix(unitinc))
    colnames(est) <- c("est", "lower", "upper")
    
    # Convert estimates into % change
    percchange <- (est - 1)*100
    
    # Store results in list
    result <- list(
      modmain = modmain,
      estRR = est,
      estperc = percchange,
      outcome = outcome
    )
    return(result)
  })
  names(causes_results) <- outcomes
  return(causes_results)
}) ; names(mainlist) <- names(dlist)

# -------------
# Pool West Midlands sites
# -------------

mainlist[["wmid_pool"]] <- lapply(outcomes, function(outcome){
  
  # Extract coefficients and variances from west midlands models 
  coefs <- unlist(lapply(birmsites, function(site) 
    mainlist[[site]][[outcome]][["modmain"]][["coefficients"]][["ufp01"]]))
  
  variances <- unlist(lapply(birmsites, function(site) 
    summary(mainlist[[site]][[outcome]][["modmain"]])$coefficients["ufp01",2]))
  
  # Standard meta analysis with fixed effects
  meta <- mixmeta(coefs ~ 1, variances, method = "fixed")
  
  # Predict change for 10,000 unit increase of UFP
  est <- as.data.frame(t(exp(predict(meta, ci = T, newdata = data.frame(inc = unitinc)))))
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


