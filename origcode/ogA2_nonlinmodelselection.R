################################################################################
# Code for the analysis in:
# 
#   Mortality risks associated with short-term exposure to ultrafine particles 
#   in London and the West Midlands
# 
#   Nenon D, Fuller G, Masselot P, Gasparrini, A.
#   Environmental Epidemiology - 2025
#
#
# Model selection for knot placement in secondary analysis
#
################################################################################

testparamslist <- list(
  lin = list(fun = "lin"),
  bsd3 = list(fun = "bs", knots = kufp, degree = 3,  Boundary.knots = bdkufp),
  bsd2 = list(fun = "bs", knots = kufp, degree = 2,  Boundary.knots = bdkufp),
  ns1 = list(fun = "ns", knots = kufp, Boundary.knots = bdkufp),
  ns2 = list(fun = "ns", knots = quantile(ufpdf$ufp, c(0.33,0.66), na.rm=T), Boundary.knots = bdkufp)
)

# Loop through the parameterizations
modelselectlist <- as.data.frame(t(do.call(rbind, lapply(testparamslist, function(testparams){
  # Loop through the measurement sites
  mainlist <- as.data.frame(do.call(cbind, lapply(names(dlist), function(location){
    
    # Extract data for each site
    data <- dlist[[location]]
    
    # Define spline of time
    spltime_param <- list(data$date, df=round(dfspltime*nrow(data)/365.25))
    spltime <- do.call(spltimefun, spltime_param)
    
    # Define temperature crossbasis
    ktemp <- quantile(data$tmean, c(10,75,90)/100, na.rm=T) # place knots at 10,75,90 of temperature
    argvartmean <- list(fun="bs", knots=ktemp, degree=2)
    cbtemp <- crossbasis(data$tmean, lag=lagtmean, argvar=argvartmean,
                         arglag=arglagtmean)
    
    # Loop on the outcomes
    causes_results <- do.call(cbind, lapply(outcomes, function(outcome){
      
      # Get the data for the specific outcome
      mortality <- data[[outcome]]
      
      # Define the spline for UFP based on the specified parameterization
      splufp_param_inloop <- c(testparams, list(x = data$ufp))
      splufp <- do.call(onebasis, splufp_param_inloop)
      
      # Run main model
      modselect <- glm(mortality ~ splufp + spltime + dow + holiday + cbtemp , 
                     data, family=quasipoisson)
      
      # Store results in list
      result <- QAIC(modselect)
      
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

# Get the difference between each parameterization and the reference (ref = first in testparamlist)
diflin <- modelselectdf - modelselectdf[,1]

# Look at mean and distribution of the differences
colMeans(diflin)
boxplot(diflin) ; abline(h=0)
