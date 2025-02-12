################################################################################
# UFP/mortality project


# Pooling effects from West Midlands sites (main analysis, extended lag, nonlin)

################################################################################

# Define vector of sites to be pooled
birmsites <- names(dlist)[2:3]

# -------------------
# Main analysis
# -------------------
mainlist[["wmid_pool"]] <- lapply(outcomes, function(outcome){
   
    # Extract coefficients and variances from west midlands models 
    coefs <- unlist(lapply(birmsites, function(site) 
      mainlist[[site]][[outcome]][["modmain"]][["coefficients"]][["ufp01"]]))
    
    variances <- unlist(lapply(birmsites, function(site) 
      summary(mainlist[[site]][[outcome]][["modmain"]])$coefficients["ufp01",2]))
    
    # Standard meta analysis with fixed effects
    meta <- mixmeta(coefs ~ 1, variances, method = "fixed")
    
    # Predict change for 10,000 unit increase of UFP
    est <- as.data.frame(t(exp(predict(meta, ci = T, newdata = data.frame(inc = unitinc)))))
    colnames(est) <- c("est", "lower", "upper")
    
    # Calculate as % change
    perc <- (est - 1)*100
    
    results <- list(
      modmain = meta,
      estRR = est,
      estperc = perc,
      outcome = outcome
    )
    return(results)
  }) ; names(mainlist[["wmid_pool"]]) <- outcomes

################################################################################
################################################################################
# -------------------
# Extended lag
# -------------------
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

# -------------------
# Nonlinear E-R
# -------------------
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

# Save results
#save(dlist, extlaglist, intlist, mainlist, nonlinlist, ufp, file = "results_figures/results.RData")

