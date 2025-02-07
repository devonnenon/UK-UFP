################################################################################
# UFP/mortality project


# Main analysis

################################################################################
params <- list(
  og = expression(onebasis(data$ufp, "ns", df = dfsplufp)),
  shared1 = expression(onebasis(data$ufp, "ns", knots = quantile(data$ufp, 0.5, na.rm=T))),
  shared2 = expression(onebasis(data$ufp, "ns", knots = quantile(data$ufp, c(0.33, 0.66), na.rm=T)))
)

modelselectlist <- lapply(params, function(param){
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
    
    splufp <- eval(param)
    
    # Run main model
    modmain <- glm(mortality ~ splufp + spltime + dow + holiday + cbtemp , 
                   data, family=quasipoisson)
    
    # Store results in list
    result <- list(
      QAIC = QAIC(modmain),
      outcome = outcome
    )
    return(result)
  })
  names(causes_results) <- outcomes
  return(causes_results)
}) ; names(mainlist) <- names(dlist)
return(mainlist)})

