################################################################################
# UFP/mortality project


# Main analysis

################################################################################
params <- list(
  lin = expression(onebasis(data$ufp, fun = "lin")),
  nonlin1 = expression(onebasis(data$ufp, "ns", knots = quantile(data$ufp, 0.5, na.rm=T))),
  nonlin2 = expression(onebasis(data$ufp, "ns", knots = quantile(data$ufp, c(0.33,0.66), na.rm=T)))
)

# Loop through the parameterizations
modelselectlist <- as.data.frame(t(do.call(rbind, lapply(params, function(param){
  # Loop through the measurement sites
  mainlist <- as.data.frame(do.call(cbind, lapply(names(dlist), function(location){
    
    # Extract the data for the site
    data <- dlist[[location]]
    
    # Define spline of time
    spltime <- eval(spltime_ex)
    
    # Define temperature crossbasis
    cbtemp <- eval(cbtemp_ex)
    
    # Loop on the outcomes
    causes_results <- do.call(cbind, lapply(outcomes, function(outcome){
      
      # Get the data for the specific outcome
      mortality <- data[[outcome]]
      
      # Define the spline for UFP based on the specified parameterization
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

return(mainlist)}))))


# Get the mean QAIC of each parameterization 
colMeans(modelselectlist, na.rm=T)

# Create data frame of QAICs
modelselectdf <- as.data.frame(modelselectlist)

# Df of the difference between the linear and the various nonlinear models (for comparison)
diflin <- modelselectdf - modelselectdf$lin

# Look at mean and distribution of the differences
colMeans(diflin)
boxplot(diflin)
