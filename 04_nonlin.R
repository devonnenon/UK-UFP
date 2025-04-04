################################################################################
# UFP/mortality project


# Secondary analyses: extended lag, nonlinear E-R, interrupted TS

################################################################################

# Get the list of location names
locations <- names(dlist)

# Initialize empty lists
nonlinlist <- list()

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
  
  # Define linear function for later plotting
  linufp <- onebasis(data$ufp01, fun = "lin")
  
  outcomes <- c("nonext", "cvd", "resp")
  outcomemodels <- lapply(outcomes, function(outcome) {
    #outcome <- "nonext"
    # Pull previous model for this location and outcome
    modmain <- mainlist[[location]][[outcome]][["modmain"]]
    
    # MODEL WITH NONLINEAR E-R
    modspl <- update(modmain, . ~ . - ufp01 + splufp)
    
    # PREDICT ESTIMATE FOR A 10,000-UNIT INCREASE
    cpspl <- crosspred(splufp, modspl, cen=0, at = preds)
    
    # Linear model for later plotting
    modlin <- update(modmain, . ~ . - ufp01 + linufp)
    
    cplin <- crosspred(linufp, modlin, cen = 0, at = preds)
    
    nonlin <- list(
      splufp = splufp,
      modspl = modspl,
      cpspl = cpspl,
      modlin = modlin,
      cplin = cplin
    )
    
    return(nonlin)
  })
  names(outcomemodels) <- outcomes
  nonlinlist[[i]] <- outcomemodels
} ; names(nonlinlist) <- names(dlist)

# -------------
# Pool West Midlands sites
# -------------

nonlinlist[["wmid_pool"]] <- lapply(outcomes, function(outcome){
  coefs <- drop(t(sapply(birmsites, function(site) {
    allcoefs <- nonlinlist[[site]][[outcome]][["modspl"]][["coefficients"]]
    splufpind <- grep("splufp", names(allcoefs))
    coefs <- allcoefs[splufpind]
    return(coefs)
  })))
  
  coefslin <- drop(t(sapply(birmsites, function(site) {
    allcoefs <- nonlinlist[[site]][[outcome]][["modlin"]][["coefficients"]]
    linufpind <- grep("linufp", names(allcoefs))
    coefs <- allcoefs[linufpind]
    return(coefs)
  })))
  
  variances <- lapply(birmsites, function(site) {
    allcoefs <- nonlinlist[[site]][[outcome]][["modspl"]][["coefficients"]]
    splufpind <- grep("splufp", names(allcoefs))
    v <- vcov(nonlinlist[[site]][[outcome]][["modspl"]])[splufpind, splufpind]
  })
  
  varianceslin <- lapply(birmsites, function(site) {
    allcoefs <- nonlinlist[[site]][[outcome]][["modlin"]][["coefficients"]]
    linufpind <- grep("linufp", names(allcoefs))
    v <- vcov(nonlinlist[[site]][[outcome]][["modlin"]])[linufpind, linufpind]
  })
  
  # Standard meta analysis with fixed effects
  meta <- mixmeta(coefs ~ 1, variances, method = "fixed")
  
  metalin <- mixmeta(coefslin ~ 1, varianceslin, method = "fixed")
  
  # Get full UFP distribution in W. Midlands
  data <- rbind(dlist[["birmcen"]], dlist[["birmtyb"]])
  
  # Define spline for prediction
  splufp <- eval(splufp_ex)
  
  linufp <- onebasis(data$ufp01, fun = "lin")
  
  # Predict 
  cpspl <- crosspred(splufp, coef = coef(meta), vcov = vcov(meta),
                     model.link = "log", cen = 0, at = preds)
  
  cplin <- crosspred(linufp, coef = coef(metalin), vcov = vcov(metalin),
                     model.link = "log", cen = 0, at = preds)
  
  # Store results
  results <- list(modspl = meta, cpspl = cpspl, modlin = metalin, cplin = cplin)
  return(results)
}) ; names(nonlinlist[["wmid_pool"]]) <- outcomes
