################################################################################
# UFP/mortality project


# Tables

################################################################################

locationsplot <- c("nkens", "wmid_pool")
locationnames <- c("London", "WestMidlands")
names(locationsplot) <- locationnames
outcomes <- c("nonext", "cvd", "resp")


# ----------------
# Extended lags
# ----------------
tabextlag <- table2df <- do.call(rbind, lapply(locationsplot, function(location){
  outcomes_res <- do.call(rbind, lapply(outcomes, function(outcome){
    cumulest <- signif(extlaglist[[location]][[outcome]][["estperc"]][,1], 3)
    cumullb <- signif(extlaglist[[location]][[outcome]][["estperc"]][,2], 3)
    cumulub <- signif(extlaglist[[location]][[outcome]][["estperc"]][,3], 3)
    cumrow <- paste0(cumulest, " (",cumullb,",",cumulub,")")
    est <- signif(extlaglist[[location]][[outcome]][["tabperc"]][,1], 3)
    lb <- signif(extlaglist[[location]][[outcome]][["tabperc"]][,2], 3)
    ub <- signif(extlaglist[[location]][[outcome]][["tabperc"]][,3], 3)
    row <- paste0(est, " (",lb,",",ub,")")
    row <- c(cumrow, row)
    return(row)
  }))
  rownames(outcomes_res) <- paste(outcomes, location) 
  return(outcomes_res)
})) ; colnames(tabextlag) <- c("net", "lag0", "lag1", "lag2", "lag3", "lag4", "lag5")

noquote(tabextlag)


# --------------------
# Interrupted analysis 
# --------------------
locationsint <- c("nkens", "birmcen")

tabint <- do.call(rbind, lapply(locationsint, function(location) {
  outcomesres <- do.call(rbind, lapply(outcomes, function(outcome){
    data <- as.data.frame(intlist[[location]][[outcome]][["estperc"]])
    data$p <- c(NA, intlist[[location]][[outcome]][["p_diff"]][2,5])
    rownames(data) <- c(paste0(location, outcome, "pre"), 
                        paste0(location, outcome, "post"))
    return(data)
  }))
}))
tabint
