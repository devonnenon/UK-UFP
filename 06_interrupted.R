################################################################################
# UFP/mortality project


# Secondary analyses: extended lag, nonlinear E-R, interrupted TS

################################################################################

# Initialize empty list
intlist <- list()

# Loop on the different locations
for(location in names(dlist[names(dlist) != "birmtyb"])){
  print(location)
  data <- dlist[[location]]
  
  # Redefine parameters from main model
  # Define spline of time
  spltime_param <- list(data$date, df=round(dfspltime*nrow(data)/365.25))
  spltime <- do.call(spltimefun, spltime_param)
  
  # Define temperature crossbasis
  ktemp <- quantile(data$tmean, c(10,75,90)/100, na.rm=T) # place knots at 10,75,90 of temperature
  argvartmean <- list(fun="bs", knots=ktemp, degree=2)
  cbtemp <- crossbasis(data$tmean, lag=lagtmean, argvar=argvartmean,
                       arglag=arglagtmean)
  
  # Define indicator for 2008 change point (new)
  data$post2008 <- year(data$date) >= 2008
  
  outcomes <- c("nonext", "cvd", "resp")
  outcomemodels <- lapply(outcomes, function(outcome) {
    #outcome <- "nonext"
    # Pull previous model for this location and outcome
    modmain <- mainlist[[location]][[outcome]][["modmain"]]
    
    # Pull temperature crossbasis from previous model
    cbtemp <- mainlist[[location]][[outcome]][["cbtemp"]]
    
    # Rerun model with interaction with change point indicator
    modpost <- update(modmain, . ~ . - ufp01 + ufp01:post2008)
    
    # Extract RR and 95% CI for X unit increase (defined in params)
    est <- ci.exp(modpost, subset="ufp01", ctr.mat=diag(10000,2))
    colnames(est) <- c("est", "lower", "upper")
    
    # Convert estimates into % change
    percchange <- (est - 1)*100
    
    # Test of a difference for interaction
    anova <- anova(modmain, modpost, test="Chisq")
    anovap <- anova$`Pr(>Chi)`[2]
    
    # Store results
    interrupted <- list(
      modpost = modpost,
      estRR = est,
      estperc = percchange,
      p_diff = anova
    )
    
    return(interrupted)
  })
  names(outcomemodels) <- outcomes
  intlist[[location]] <- outcomemodels
}

