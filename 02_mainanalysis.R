################################################################################
# UFP/mortality project


# Main analysis

################################################################################

# Loop through the measurement sites
mainlist <- lapply(names(dlist), function(location){
  data <- dlist[[location]]

  # Define spline of time
  spltime <- ns(data$date, df=round(dfspltime*nrow(data)/365.25))
  
  # Define temperature crossbasis
  ktemp <- quantile(data$tmean, c(10, 75, 90)/100, na.rm = T)
  cbtemp <- crossbasis(data$tmean, lag = 3, argvar = list(fun = "ns", knots = ktemp),
                       arglag = list(fun = "strata", breaks = 1))
  
# to here *****
  
  # Define mortality outcomes to be tested 
  outcomes <- c("nonext", "cvd", "resp")
  
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
})
names(mainlist) <- names(dlist)

