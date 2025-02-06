################################################################################
# UFP/mortality project


# Secondary analyses: extended lag, nonlinear E-R, interrupted TS

################################################################################

# Get the list of location names
locations <- names(dlist)

# Initialize empty lists
secondarylist <- list()
extlaglist <- list()
nonlinlist <- list()
intlist <- list()

# -------------
# Extended lags 
# -------------
# Loop on the different locations
for(i in 1:length(locations)){
  print(i)
  location <- locations[[i]]
  data <- dlist[[location]]
  
  # Redefine parameters from main model
  # Define spline of time
  spltime <- eval(spltime_ex)
 
  # Define temperature crossbasis
  cbtemp <- eval(cbtemp_ex)
  
  # Define cross basis for ufp (new)
  cbufp <- crossbasis(data$ufp, lag = lagufp, argvar = list(fun = "lin"),
                      arglag=list(fun = "integer"))
 
  outcomes <- c("nonext", "cvd", "resp")
  outcomemodels <- lapply(outcomes, function(outcome) {
    #outcome <- "nonext"
    # Pull previous model for this location and outcome
    modmain <- mainlist[[location]][[outcome]][["modmain"]]
    
    #--------------------------
    # MODEL WITH EXTENDED LAGS
    modlag <- update(modmain, . ~ . - ufp01 + cbufp)
    
    # Predict estimate for a X unit increase (as defined in params)
    cplag <- crosspred(cbufp, modlag, at=unitinc)
    
    # Extract RR and 95% CI for X unit increase (defined in params)
    lagsest <- as.matrix(t(with(cplag, c(allRRfit, allRRlow, allRRhigh))))
    colnames(lagsest) <- c("est", "lower", "upper")
    
    # Calculate % change from RR
    lagspercchange <- (lagsest - 1)*100
    
    # Generate table of the individual lags
    tablag <- with(cplag,t(rbind(matRRfit,matRRlow,matRRhigh)))
    colnames(tablag) <- c("est", "lower", "upper")
    
    # Table in % change format
    tablagperc <- (tablag - 1)*100
    
    extlags <- list(
      modlag = modlag,
      estRR = lagsest,
      estperc = lagspercchange,
      tabRR = tablag,
      tabperc = tablagperc
    )
    
    return(extlags)
  })
  names(outcomemodels) <- outcomes
  extlaglist[[i]] <- outcomemodels
} ; names(extlaglist) <- names(dlist)


# -------------
# Nonlinear E-R
# -------------
# Loop on the different locations
for(i in 1:length(locations)){
  print(i)
  location <- locations[[i]]
  data <- dlist[[location]]
  
  # Redefine parameters from main model
  # Define spline of time
  spltime <- eval(spltime_ex)
  
  # Define temperature crossbasis
  cbtemp <- eval(cbtemp_ex)
  
  # Define spline for ufp (new)
  splufp <- eval(splufp_ex)
  
  outcomes <- c("nonext", "cvd", "resp")
  outcomemodels <- lapply(outcomes, function(outcome) {
    #outcome <- "nonext"
    # Pull previous model for this location and outcome
    modmain <- mainlist[[location]][[outcome]][["modmain"]]
    
    # MODEL WITH NONLINEAR E-R
    modspl <- update(modmain, . ~ . - ufp01 + splufp)
    
    # PREDICT ESTIMATE FOR A 10,000-UNIT INCREASE
    cpspl <- crosspred(splufp, modspl, cen=0, at = unitinc)
    
    nonlin <- list(
      modspl = modspl,
      cpspl = cpspl
    )
    
    return(nonlin)
  })
  names(outcomemodels) <- outcomes
  nonlinlist[[i]] <- outcomemodels
} ; names(nonlinlist) <- names(dlist)

# -------------
# Interrupted (only in nkens and birmcen)
# -------------
# Loop on the different locations
for(i in 1:(length(locations)-1)){
  print(i)
  location <- locations[[i]]
  data <- dlist[[location]]
  
  # Redefine parameters from main model
  # Define spline of time
  spltime <- eval(spltime_ex)
  
  # Define temperature crossbasis
  cbtemp <- eval(cbtemp_ex)
  
  # Define indicator for 2008 change point (new)
  data$post2008 <- year(data$date) >= 2008
  
  outcomes <- c("nonext", "cvd", "resp")
  outcomemodels <- lapply(outcomes, function(outcome) {
    #outcome <- "nonext"
    # Pull previous model for this location and outcome
    modmain <- mainlist[[location]][[outcome]][["modmain"]]
    
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
  intlist[[i]] <- outcomemodels
} ; names(intlist) <- names(dlist[1:2])

