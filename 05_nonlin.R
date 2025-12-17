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
# Secondary analysis: nonlinear exposure response 
#
################################################################################

# Initialize empty lists
nonlinlist <- list()

# Loop on the different locations
for(location in names(dlist)){
  print(location)
  
  # Extract location data
  data <- dlist[[location]]
  
  # Redefine parameters from main model:
  # Define spline of time
  spltime_param <- list(data$date, df=round(dfspltime*nrow(data)/365.25))
  spltime <- do.call(spltimefun, spltime_param)
  
  # Define temperature crossbasis
  ktemp <- quantile(data$tmean, c(10,75,90)/100, na.rm=T) # place knots at 10,75,90 of temperature
  argvartmean <- list(fun="bs", knots=ktemp, degree=2)
  cbtemp <- crossbasis(data$tmean, lag=lagtmean, argvar=argvartmean,
                       arglag=arglagtmean)
  
  # Define spline for ufp
  splufp_param_inloop <- c(splufpfun, list(x = data$ufp), splufp_param)
  splufp <- do.call(onebasis, splufp_param_inloop)
  
  # Define linear function (for later plotting)
  linufp <- onebasis(data$ufp01, fun = "lin")
  
  # Pull previous model for this location
  modmain <- mainlist[[location]][["modmain"]]
  
  # Pull temperature crossbasis from previous model
  cbtemp <- mainlist[[location]][["cbtemp"]]
  
  #-------------------------
  # MODEL WITH NONLINEAR E-R - change moving average to spline term
  modspl <- update(modmain, . ~ . - ufp01 + splufp)
  
  # Predict exposure response curve 
  cpspl <- crosspred(splufp, modspl, cen=0, at = preds)
  
  # Rerun linear model using onebasis (for later plotting)
  modlin <- update(modmain, . ~ . - ufp01 + linufp)
  
  # Predict linear exposure response curve
  cplin <- crosspred(linufp, modlin, cen = 0, at = preds)
  
  # Store results in list 
  nonlinlist[[location]] <- list(
    splufp = splufp,
    modspl = modspl,
    cpspl = cpspl,
    modlin = modlin,
    cplin = cplin
  )
}

# -------------
# Pool West Midlands sites
# -------------

# Extract coefficients and variances from west midlands models 
coefs <- drop(t(sapply(birmsites, function(site) {
  allcoefs <- nonlinlist[[site]][["modspl"]][["coefficients"]]
  splufpind <- grep("splufp", names(allcoefs))
  coefs <- allcoefs[splufpind]
  return(coefs)
})))

coefslin <- drop(t(sapply(birmsites, function(site) {
  allcoefs <- nonlinlist[[site]][["modlin"]][["coefficients"]]
  linufpind <- grep("linufp", names(allcoefs))
  coefs <- allcoefs[linufpind]
  return(coefs)
})))

variances <- lapply(birmsites, function(site) {
  allcoefs <- nonlinlist[[site]][["modspl"]][["coefficients"]]
  splufpind <- grep("splufp", names(allcoefs))
  v <- vcov(nonlinlist[[site]][["modspl"]])[splufpind, splufpind]
})

varianceslin <- lapply(birmsites, function(site) {
  allcoefs <- nonlinlist[[site]][["modlin"]][["coefficients"]]
  linufpind <- grep("linufp", names(allcoefs))
  v <- vcov(nonlinlist[[site]][["modlin"]])[linufpind, linufpind]
})

# Standard meta analysis with fixed effects (for both models)
meta <- mixmeta(coefs ~ 1, variances, method = "fixed")
metalin <- mixmeta(coefslin ~ 1, varianceslin, method = "fixed")

# Get full UFP distribution in W. Midlands for prediction
data <- rbind(dlist[["birmcen"]], dlist[["birmtyb"]])

# Define spline and linear basis 
splufp <- do.call(onebasis, c(splufpfun, list(x = data$ufp), splufp_param))
linufp <- onebasis(data$ufp01, fun = "lin")

# Predict both ERFs
cpspl <- crosspred(splufp, coef = coef(meta), vcov = vcov(meta),
                   model.link = "log", cen = 0, at = preds)

cplin <- crosspred(linufp, coef = coef(metalin), vcov = vcov(metalin),
                   model.link = "log", cen = 0, at = preds)

# Store results
nonlinlist[["wmid_pool"]] <- list(
  splufp = splufp,
  modspl = meta,
  cpspl = cpspl,
  modlin = modlin,
  cplin = cplin
  )

# Remove intermediate objects
rm(coefs, coefslin, variances, varianceslin, meta, metalin, data, splufp,
   linufp, cpspl, cplin, argvartmean, cbtemp, ktemp, location, modlin,
   modmain, modspl, spltime, spltime_param, splufp_param_inloop)

