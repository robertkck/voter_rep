# Limiting loss after allocation
## Fix minimum and maximum bounds. Minimum is not respected, max always looses

limitloss <- function(rep, rep_scen, rep_scen_exact){
  diffs_rep_scen <- rep_scen - rep
  remainder <- abs(sum(diffs_rep_scen[diffs_rep_scen < -1] +1 ))
  possible <- sum(diffs_rep_scen[diffs_rep_scen > -1] + 1 )
  if (remainder > possible) {
    showNotification(paste("Limiting loss not feasible. Not enough seats to distribute."), type = "error", duration = 2)
  }
  validate(
    need(remainder <= possible, "")
  )
  rep_scen[diffs_rep_scen < -1] <-  rep[diffs_rep_scen < -1] - 1
  if (remainder > 0) {
    for (i in 1:remainder) {
      indicator <- rep_scen - rep_scen_exact
      # print(indicator)
      indicator[rep_scen - rep < 0] <- -100
      rep_scen[which.max(indicator)] <- rep_scen[which.max(indicator)] - 1
      # data$rep_scen[which.max(data$rep_scen[data$diffs_rep_scen > 0] - data$rep_scen_exact[data$diffs_rep_scen > 0])] <- data$rep_scen[which.max(data$rep_scen - data$rep_scen_exact)] - 1
      # print(i)
      # print(sum(data$rep_scen))
      # print(which.max(indicator))
    }
  }
  return(list(rep_scen = rep_scen, r = remainder))
}