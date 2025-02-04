

#secondarylist <- lapply(names(dlist), function(location){
locations <- names(dlist)

secondarylist <- list()

for(i in 1:length(locations)){
  print(i)
  location <- locations[[i]]
  data <- dlist[[location]]
  
  # Define crossbasis for ufp (extended lag analysis)
  cbufp <- crossbasis(data$ufp, lag = lagufp, argvar = list(fun = "lin"),
                      arglag=list(fun = "integer"))
  
  #  # Redefine parameters from main model
  #  # Define spline of time
  spltime <- ns(data$date, df=round(dfspltime*nrow(data)/365.25))
 
  #  # Define temperature crossbasis
  ktemp <- quantile(data$tmean, c(10, 75, 90)/100, na.rm = T)
  cbtemp <- crossbasis(data$tmean, lag = 3, argvar = list(fun = "ns", knots = ktemp),
                       arglag = list(fun = "strata", breaks = 1))
  
  # Define spline for ufp (for nonlinear E-R)
  splufp <- onebasis(data$ufp, "ns", df = dfsplufp)
 
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
  
  #--------------------------
  # MODEL WITH NONLINEAR E-R
  modspl <- update(modmain, . ~ . - ufp01 + splufp)
  
  # PREDICT ESTIMATE FOR A 10,000-UNIT INCREASE
  cpspl <- crosspred(splufp, modspl, cen=0)
  
  nonlin <- list(
    modspl = modspl,
    cpspl = cpspl
  )
  
  #--------------------------
  # INTERRUPTED ANALYSIS
  if (i != 3) { # for birmingham tyburn, bypass this analysis (change point outside study period for this site)
    
    # Define indicator for 2008 change point
    data$post2008 <- year(data$date) >= 2008
    
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
    
    # Store results from each analysis
    results <- list(
      extlags = extlags,
      nonlin = nonlin,
      interrupted = interrupted
  ) 
  } else {
    results <- list(
      extlags = extlags,
      nonlin = nonlin
    )
  }
  
  return(results)
  })
  names(outcomemodels) <- outcomes
  secondarylist[[i]] <- outcomemodels
}

names(secondarylist) <- names(dlist)

