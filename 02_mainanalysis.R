################################################################################
# UFP/mortality project


# Main analysis

################################################################################

# Loop through the measurement sites
mainlist <- lapply(names(dlist), function(location){
  
  data <- dlist[[location]]

  # Define spline of time
  spltime <- eval(spltime_ex)
  
  # Define temperature crossbasis
  cbtemp <- eval(cbtemp_ex)
  
  # Loop on the outcomes
  causes_results <- lapply(outcomes, function(outcome){
    
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

