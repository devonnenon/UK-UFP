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
# Secondary analysis: effect modification of 2007 fuel policy - interrupted TS
#
################################################################################

# Initialize empty list
intlist <- list()

# Loop on the different locations - only kensington and birmcen
for(location in names(dlist[names(dlist) != "birmtyb"])){
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
  
  # Define indicator for 2008 change point in the data
  data$post2008 <- year(data$date) >= 2008
  
  # Pull previous model for this location
  modmain <- mainlist[[location]][["modmain"]]
  
  # Pull temperature crossbasis from previous model
  cbtemp <- mainlist[[location]][["cbtemp"]]
  
  # Rerun model with interaction with change point indicator
  modpost <- update(modmain, . ~ . - ufp01 + ufp01:post2008)
  
  # Extract RR and 95% CI for X unit increase (as defined in params)
  est <- ci.exp(modpost, subset="ufp01", ctr.mat=diag(10000,2))
  colnames(est) <- c("est", "lower", "upper")
  
  # Test of a difference for interaction
  anova <- anova(modmain, modpost, test="Chisq")
  anovap <- anova$`Pr(>Chi)`[2]
  
  # Store results
  intlist[[location]] <- list(
    modpost = modpost,
    estRR = est,
    p_diff = anova
  )
}

# Remove unneeded objects
rm(anova, anovap, argvartmean, cbtemp, data, est, ktemp, location, modmain, 
   modpost, spltime, spltime_param)
