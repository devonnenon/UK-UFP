################################################################################
# UFP/mortality project


# Main analysis

################################################################################
params <- list(
  og = expression(onebasis(data$ufp, "ns", df = dfsplufp)),
  lin = expression(onebasis(data$ufp, fun = "lin")),
  shared1 = expression(onebasis(data$ufp, "ns", knots = quantile(data$ufp, 0.5, na.rm=T))),#
  shared140 = expression(onebasis(data$ufp, "ns", knots = quantile(data$ufp, 0.4, na.rm=T))),
  shared150 = expression(onebasis(data$ufp, "ns", knots = quantile(data$ufp, 0.6, na.rm=T))),
  shared125 = expression(onebasis(data$ufp, "ns", knots = quantile(data$ufp, 0.25, na.rm=T))),
  shared2 = expression(onebasis(data$ufp, "ns", knots = quantile(data$ufp, c(0.33, 0.66), na.rm=T))),
  bs1 = expression(onebasis(data$ufp, "bs", knots = quantile(data$ufp, 0.5, na.rm=T))),
  bs2 = expression(onebasis(data$ufp, "bs", knots = quantile(data$ufp, c(0.33, 0.66), na.rm=T))),
  ps5 = expression(onebasis(data$ufp, "ps", df = 5)),
  ps7 = expression(onebasis(data$ufp, "ps", df = 7))
)

modelselectlist <- t(do.call(rbind, lapply(params, function(param){
# Loop through the measurement sites
mainlist <- as.data.frame(do.call(cbind, 
                                  lapply(names(dlist), function(location){

  data <- dlist[[location]]
  
  # Define spline of time
  spltime <- eval(spltime_ex)
  
  # Define temperature crossbasis
  cbtemp <- eval(cbtemp_ex)

  # Loop on the outcomes
  causes_results <- do.call(cbind, lapply(outcomes, function(outcome){
    
    mortality <- data[[outcome]]
    
    splufp <- eval(param)
    
    # Run main model
    modmain <- glm(mortality ~ splufp + spltime + dow + holiday + cbtemp , 
                   data, family=quasipoisson)
    
    # Store results in list
    result <- QAIC(modmain)

    return(result)
  }))
  names(causes_results) <- outcomes
  return(causes_results)
}))) ; names(mainlist) <- names(dlist)
return(mainlist)})))

colMeans(modelselectlist, na.rm=T)

