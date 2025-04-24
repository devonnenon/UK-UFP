################################################################################
# UFP/mortality project


# Tables

################################################################################

locationsplot <- c("nkens", "wmid_pool")
locationnames <- c("London", "WestMidlands")
names(locationsplot) <- locationnames
outcomes <- c("nonext", "cvd", "resp")

# ----------------
# Main results
# ----------------
tabmain <- do.call(rbind, lapply(names(mainlist), function(location){
  outcomeres <- do.call(rbind, lapply(outcomes, function(outcome){
    est <- round(mainlist[[location]][[outcome]][["estRR"]],4)
    return(data.frame(location = location, outcome = outcome, 
                      RR = paste0(est[1]," (", est[2], " to ", est[3], ")")))
  }))
  return(outcomeres)
})) %>% pivot_wider(names_from = outcome, values_from = RR)

print(tabmain, quote = F)

write.csv(tabmain, file = "results_figures/maintable.csv")

# ----------------
# Extended lags
# ----------------
tabextlag <- do.call(rbind, lapply(names(mainlist), function(location){
  outcomes_res <- do.call(rbind, lapply(outcomes, function(outcome){
    cumulest <- round(extlaglist[[location]][[outcome]][["estRR"]][,1], 3)
    cumullb <- round(extlaglist[[location]][[outcome]][["estRR"]][,2], 3)
    cumulub <- round(extlaglist[[location]][[outcome]][["estRR"]][,3], 3)
    cumrow <- paste0(cumulest, " (",cumullb,",",cumulub,")")
    est <- round(extlaglist[[location]][[outcome]][["tabRR"]][,1], 3)
    lb <- round(extlaglist[[location]][[outcome]][["tabRR"]][,2], 3)
    ub <- round(extlaglist[[location]][[outcome]][["tabRR"]][,3], 3)
    row <- paste0(est, " (",lb,",",ub,")")
    row <- c(cumrow, row)
    return(row)
  }))
  rownames(outcomes_res) <- paste(outcomes, location) 
  return(outcomes_res)
})) ; colnames(tabextlag) <- c("net", "lag0", "lag1", "lag2", "lag3", "lag4", "lag5")

noquote(tabextlag)

write.csv(tabextlag, file = "results_figures/extlagtable.csv")
# --------------------
# Interrupted analysis 
# --------------------
locationsint <- c("nkens", "birmcen")

tabint <- do.call(rbind, lapply(locationsint, function(location) {
  outcomesres <- do.call(rbind, lapply(outcomes, function(outcome){
    data <- as.data.frame(intlist[[location]][[outcome]][["estRR"]])
    data$p <- c(NA, intlist[[location]][[outcome]][["p_diff"]][2,5])
    rownames(data) <- c(paste0(location, outcome, "pre"), 
                        paste0(location, outcome, "post"))
    return(data)
  }))
}))

tabint <- do.call(rbind, lapply(locationsint, function(location){
  outcomesres <- do.call(rbind, lapply(outcomes, function(outcome){
    results <- round(as.data.frame(intlist[[location]][[outcome]][["estRR"]]), 3)
    row <- data.frame(location = location, outcome = outcome, 
                      pre = paste0(results[1,"est"]," (", results[1,"lower"], ", ",
                      results[1,"upper"],")"), post = paste0(results[2,"est"]," (", 
                      results[2,"lower"], ", ",results[2,"upper"],")"), 
                      p_value = signif(intlist[[location]][[outcome]][["p_diff"]][2,5], 2))
    return(row)
  }))# ; rownames(outcomesres) <- outcomes
}))# ; names(tabint) <- locationsint

tabint

write.csv(tabint, file = "results_figures/interruptedtable.csv")
