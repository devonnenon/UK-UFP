################################################################################
# UFP/mortality project


# Secondary analyses: extended lag, nonlinear E-R, interrupted TS

################################################################################

secondarylist <- lapply(names(dlist), function(location){
 
#  location <- "birmcen"
   data2 <- dlist[[location]]
   
 #  #data2 <- dlist[[1]]
 # 
 #  # Create moving average of UFP
   data2$ufp01 <- runMean(data2$ufp, 0:1)

 #  # Trim to exclude periods of missing data2 at beginning or end
   subrange <- range(seq(nrow(data2))[!is.na(data2$ufp)])
   data2 <- data2[subrange[1]:subrange[2],]
 # 
 #  # Define crossbasis for ufp (extended lag analysis)
   cbufp <- crossbasis(data2$ufp, lag = lagufp, argvar = list(fun = "lin"),
                       arglag=list(fun = "integer"))
   
 # 
 #  # Define spline for ufp (for nonlinear E-R)
   splufp <- onebasis(data2$ufp, "ns", df = dfsplufp)
 # 
 #  # Define indicator for change point (for interrupted analysis)
   data2$post2008 <- ifelse(year(data2$date) <= 2008, 0, 1)
 # 
 #  # Redefine parameters from main model
 #  # Define spline of time
   spltime <- ns(data2$date, df=round(dfspltime*nrow(data2)/365.25))
 # 
 #  # Define temperature crossbasis
   ktemp <- quantile(data2$tmean, c(10, 75, 90)/100, na.rm = T)
   cbtemp <- crossbasis(data2$tmean, lag = 3, argvar = list(fun = "ns", knots = ktemp),
                        arglag = list(fun = "strata", breaks = 1))
 # 
 #  # Define mortality outcomes to be tested
 #  outcomes <- c("nonext", "cvd", "resp")
   outcome <- "nonext"
 #  # Loop on the outcomes
 #  #causes_results <- lapply(outcomes, function(outcome){
 #    # outcome <- "cvd"
 #    # Pull previous model
     modmain <- mainlist[[location]][[outcome]][["modmain"]]
 
 #    #--------------------------
 #    # MODEL WITH LAGS
 #    return(cbufp)
     mortality <- data2[[outcome]]
    # modlag <- update(modmain, . ~ . - ufp01 + cbufp)
    
     #modlag <- glm(mortality ~ cbufp + spltime + dow + holiday + cbtemp , 
   #                 data2, family=quasipoisson)
 
 #    # Predict estimate for a X unit increase (as defined in params)
 #    cplag <- crosspred(cbufp, modlag, at=unitinc)
 # 
 #    # Extract RR and 95% CI for X unit increase (defined in params)
 #    lagsest <- as.matrix(t(with(cplag, c(allRRfit, allRRlow, allRRhigh))))
 #    colnames(lagsest) <- c("est", "lower", "upper")
 # 
 #    # Calculate % change from RR
 #    lagspercchange <- (lagsest - 1)*100
 # 
 #    # Generate table of the individual lags
 #    tablag <- with(cplag,t(rbind(matRRfit,matRRlow,matRRhigh)))
 #    colnames(tablag) <- c("est", "lower", "upper")
 # 
 #    # Table in % change format
 #    tablagperc <- (tablag - 1)*100
 # 
 #    extlags <- list(
 #      modlag = modlag,
 #      estRR = lagsest,
 #      estperc = lagspercchange,
 #      tabRR = tablag,
 #      tabperc = tablagperc
 #    )
 # 
 #    #--------------------------
 #    # MODEL WITH NONLINEAR E-R
     modspl <- update(modmain, . ~ . - ufp01 + splufp)
 # 
 #    # PREDICT ESTIMATE FOR A 10,000-UNIT INCREASE
 #    cpspl <- crosspred(splufp, modspl, cen=0)
 # 
 #  return(extlags)
 # })
  # Extend lag to 5 days
  #mod <- mainlist[]
 # return(causes_results)
})


rm(cbtemp, cbufp, cplag, cpspl, data2, extlags, lagsest, lagspercchange, 
   modlag, modspl, spltime, splufp, tablag, tablagperc, ktemp, subrange)
