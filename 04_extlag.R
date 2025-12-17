################################################################################
# Reproducible code for the analysis of non-accidental mortality in:
# 
#   Mortality risks associated with short-term exposure to ultrafine particles 
#   in London and the West Midlands
# 
#   Nenon D, Fuller G, Masselot P, Gasparrini, A.
#   Environmental Epidemiology - 2025
#
#
# Secondary analysis: extended lag
#
################################################################################

# Initialize empty list
extlaglist <- list()

# Loop on the different locations
for(location in names(dlist)){
  print(location)
  
  # Load location data 
  data <- dlist[[location]]
  
  # Redefine parameters from main model:
  # Define spline of time
  spltime_param <- list(data$date, df=round(dfspltime*nrow(data)/365.25))
  spltime <- do.call(spltimefun, spltime_param)
  
  # Define cross basis for ufp (new)
  cbufp <- crossbasis(data$ufp, lag = lagufp, argvar = argvarufp,
                      arglag= arglagufp)
  
  # Pull previous model for this location
  modmain <- mainlist[[location]][["modmain"]]
  
  # Pull temperature crossbasis from previous model
  cbtemp <- mainlist[[location]][["cbtemp"]]
  
  #--------------------------
  # MODEL WITH EXTENDED LAGS - change moving average to crossbasis 
  modlag <- update(modmain, . ~ . - ufp01 + cbufp)
  
  # Predict estimate for a X unit increase (as defined in params)
  cplag <- crosspred(cbufp, modlag, at=unitinc)
  
  # Extract RR and 95% CI for X unit increase (as defined in params)
  lagsest <- as.matrix(t(with(cplag, c(allRRfit, allRRlow, allRRhigh))))
  colnames(lagsest) <- c("est", "lower", "upper")
  
  # Generate table of the individual lags
  tablag <- with(cplag,t(rbind(matRRfit,matRRlow,matRRhigh)))
  colnames(tablag) <- c("est", "lower", "upper")
  
  # Store results
  extlaglist[[location]] <- list(
    modlag = modlag,
    estRR = lagsest,
    tabRR = tablag
    )
} 

# -------------
# Pool West Midlands sites
# -------------

# Extract coefficients and variances from west midlands models 
coefs <- drop(t(sapply(birmsites, function(site) {
  allcoefs <- extlaglist[[site]][["modlag"]][["coefficients"]]
  # Find indicies of relevant coefficients 
  cbufpind <- grep("cbufp", names(allcoefs))
  # Extract just the relevant coefficients 
  coefs <- allcoefs[cbufpind]
  return(coefs)
  })))

variances <- lapply(birmsites, function(site) {
  allcoefs <- extlaglist[[site]][["modlag"]][["coefficients"]]
  # Extract just relevant variances by index
  cbufpind <- grep("cbufp", names(allcoefs))
  v <- vcov(extlaglist[[site]][["modlag"]])[cbufpind, cbufpind]
  })
  
# Standard meta analysis with fixed effects
meta <- mixmeta(coefs ~ 1, variances, method = "fixed")

# Get full UFP distribution in W. Midlands for prediction
wmidufp <- rbind(dlist[["birmcen"]], dlist[["birmtyb"]])

# Define crossbasis for prediction
cbufp <- crossbasis(wmidufp$ufp, lag = lagufp, argvar = list(fun = "lin"),
                    arglag=list(fun = "integer"))

# Predict estimate for a X unit increase (as defined in params)
cplag <- crosspred(cbufp, coef = coef(meta), vcov = vcov(meta), model.link = "log", 
                   at=unitinc)

# Extract RR and 95% CI for X unit increase (as defined in params)
lagsest <- as.matrix(t(with(cplag, c(allRRfit, allRRlow, allRRhigh))))
colnames(lagsest) <- c("est", "lower", "upper")


# Generate table of the individual lags
tablag <- with(cplag,t(rbind(matRRfit,matRRlow,matRRhigh)))
colnames(tablag) <- c("est", "lower", "upper")

# Store results in list
extlaglist[["wmid_pool"]] <- list(modlag = meta, estRR = lagsest, tabRR = tablag)  

# Remove unneeded objects
rm(coefs, variances, meta, wmidufp, cbufp, cplag, lagsest, tablag,
   cbtemp, data, location, modlag, modmain, spltime, spltime_param)  
