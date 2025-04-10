################################################################################
# UFP/mortality project


# Secondary analyses: extended lag, nonlinear E-R, interrupted TS

################################################################################

# Get the list of location names
locations <- names(dlist)

# Initialize empty lists
extlaglist <- list()

# Loop on the different locations
for(i in 1:length(locations)){ #for loop - lapply not working for some reason
  print(i)
  location <- locations[[i]]
  data <- dlist[[location]]
  
  # Redefine parameters from main model
  # Define spline of time
  spltime_param <- list(data$date, df=round(dfspltime*nrow(data)/365.25))
  spltime <- do.call(spltimefun, spltime_param)
  
  # Define cross basis for ufp (new)
  cbufp <- crossbasis(data$ufp, lag = lagufp, argvar = argvarufp,
                      arglag= arglagufp)
  
  outcomes <- c("nonext", "cvd", "resp")
  outcomemodels <- lapply(outcomes, function(outcome) {
    #outcome <- "nonext"
    # Pull previous model for this location and outcome
    modmain <- mainlist[[location]][[outcome]][["modmain"]]
    
    # Pull temperature crossbasis from previous model
    cbtemp <- mainlist[[location]][[outcome]][["cbtemp"]]
    
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
# Pool West Midlands sites
# -------------

extlaglist[["wmid_pool"]] <- lapply(outcomes, function(outcome){
  coefs <- drop(t(sapply(birmsites, function(site) {
    allcoefs <- extlaglist[[site]][[outcome]][["modlag"]][["coefficients"]]
    cbufpind <- grep("cbufp", names(allcoefs))
    coefs <- allcoefs[cbufpind]
    return(coefs)
  })))
  
  variances <- lapply(birmsites, function(site) {
    allcoefs <- extlaglist[[site]][[outcome]][["modlag"]][["coefficients"]]
    cbufpind <- grep("cbufp", names(allcoefs))
    v <- vcov(extlaglist[[site]][[outcome]][["modlag"]])[cbufpind, cbufpind]
  })
  
  # Standard meta analysis with fixed effects
  meta <- mixmeta(coefs ~ 1, variances, method = "fixed")
  
  # Get full UFP distribution in W. Midlands
  wmidufp <- rbind(dlist[["birmcen"]], dlist[["birmtyb"]])
  
  # Define crossbasis for prediction
  cbufp <- crossbasis(wmidufp$ufp, lag = lagufp, argvar = list(fun = "lin"),
                      arglag=list(fun = "integer"))
  
  # Predict estimate for a X unit increase (as defined in params)
  cplag <- crosspred(cbufp, coef = coef(meta), vcov = vcov(meta), model.link = "log", 
                     at=unitinc)
  
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
  
  results <- list(modlag = meta, estRR = lagsest, tabRR = tablag, 
                  estperc = lagspercchange, tabperc = tablagperc)
  return(results)
}) ; names(extlaglist[["wmid_pool"]]) <- outcomes
