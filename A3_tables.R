################################################################################
# UFP/mortality project


# Tables

################################################################################

locationsplot <- c("nkens", "wmid_pool")
locationnames <- c("London", "WestMidlands")
names(locationsplot) <- locationnames
outcomes <- c("nonext", "cvd", "resp")

table2df <- do.call(rbind, lapply(locationsplot, function(location){
  outcomes_res <- do.call(rbind, lapply(outcomes, function(outcome){
    row <- mainlist[[location]][[outcome]][["estperc"]]
    return(row)
  }))
  rownames(outcomes_res) <- paste0(outcomes, names(location)) 
  return(outcomes_res)
}))

format(signif(table2df, 3), scientific = F)


tableextlag <- table2df <- do.call(rbind, lapply(locationsplot, function(location){
  outcomes_res <- do.call(rbind, lapply(outcomes, function(outcome){
    est <- signif(extlaglist[[location]][[outcome]][["tabperc"]][,1], 3)
    lb <- signif(extlaglist[[location]][[outcome]][["tabperc"]][,2], 3)
    ub <- signif(extlaglist[[location]][[outcome]][["tabperc"]][,3], 3)
    row <- c(est, lb, ub)
    return(row)
  }))
  rownames(outcomes_res) <- paste0(outcomes, names(location)) 
  return(outcomes_res)
}))
