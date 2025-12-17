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
# Results: tables
#
################################################################################

# Select locations to include in results tables and define names
locationstab <- c("kensington", "wmid_pool")
locationnames <- c("London", "WestMidlands")
names(locationstab) <- locationnames

# ----------------
# Main results
# ----------------

tabmain <- do.call(rbind, lapply(locationstab, function(location){
  est <- round(mainlist[[location]][["estRR"]],3)
  return(data.frame(location = location,
                    RR = paste0(est[1]," (", est[2], " to ", est[3], ")")))
}))

print(tabmain, quote = F)

# ----------------
# Extended lags
# ----------------
tabextlag <- do.call(rbind, lapply(locationstab, function(location){
  cumulest <- round(extlaglist[[location]][["estRR"]][,1], 3)
  cumullb <- round(extlaglist[[location]][["estRR"]][,2], 3)
  cumulub <- round(extlaglist[[location]][["estRR"]][,3], 3)
  cumrow <- paste0(cumulest, " (",cumullb,",",cumulub,")")
  est <- round(extlaglist[[location]][["tabRR"]][,1], 3)
  lb <- round(extlaglist[[location]][["tabRR"]][,2], 3)
  ub <- round(extlaglist[[location]][["tabRR"]][,3], 3)
  row <- paste0(est, " (",lb,",",ub,")")
  row <- c(cumrow, row)
  return(row)
})) ; colnames(tabextlag) <- c("net", "lag0", "lag1", "lag2", "lag3", "lag4", "lag5")

noquote(tabextlag)

# --------------------
# Interrupted analysis 
# --------------------
locationsint <- c("kensington", "birmcen")

tabint <- do.call(rbind, lapply(locationsint, function(location){
  results <- round(as.data.frame(intlist[[location]][["estRR"]]), 3)
  row <- data.frame(location = location, 
                    pre = paste0(results[1,"est"]," (", results[1,"lower"], ", ",
                                 results[1,"upper"],")"), post = paste0(results[2,"est"]," (", 
                                                                        results[2,"lower"], ", ",results[2,"upper"],")"), 
                    p_value = signif(intlist[[location]][["p_diff"]][2,5], 2))
  return(row)
}))

print(tabint)

