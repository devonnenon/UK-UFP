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
# Main analysis
#
################################################################################

# Loop through the measurement sites
mainlist <- lapply(names(dlist), function(location){
  
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

  # RUN MAIN MODEL
  modmain <- glm(nonext ~ ufp01 + spltime + dow + holiday + cbtemp + pm2501, 
                 data, family=quasipoisson)
  
  # Extract RR and 95% CI for X unit increase (as defined in params)
  est <- ci.exp(modmain, subset="ufp01", ctr.mat=matrix(unitinc))
  colnames(est) <- c("est", "lower", "upper")
  
  # Store results in list
  result <- list(
    modmain = modmain,
    estRR = est,
    cbtemp = cbtemp
  )
  
  return(result)
}) ; names(mainlist) <- names(dlist)

# -------------
# Pool West Midlands sites
# -------------

# Store site names for West Midlands
birmsites <- c("birmcen", "birmtyb")

# Extract coefficients and variances from west midlands models 
coefs <- unlist(lapply(birmsites, function(site) 
  mainlist[[site]][["modmain"]][["coefficients"]][["ufp01"]]))

variances <- unlist(lapply(birmsites, function(site) 
  vcov(mainlist[[site]][["modmain"]])["ufp01","ufp01"]))

# Standard meta analysis with fixed effects
meta <- mixmeta(coefs ~ 1, variances, method = "fixed")

# Predict change for 10,000 unit increase of UFP
est <- as.data.frame(t(exp(predict(meta, ci = T)[1,]*10000)))
colnames(est) <- c("est", "lower", "upper")

# Store results 
mainlist[["wmid_pool"]] <- list(
  modmain = meta,
  estRR = est
)

# Remove intermediate objects
rm(coefs, variances, meta, est)
