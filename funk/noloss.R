# No loss after allocation
## Fix minimum and maximum bounds. Minimum is not respected, max always looses

noloss <- function(rep, rep_scen, rep_scen_exact, m, M){
  diffs_rep_scen <- rep_scen - rep
  remainder <- abs(sum(diffs_rep_scen[diffs_rep_scen < 0]))
  possible <- sum(diffs_rep_scen[diffs_rep_scen > 0])
  if (remainder > possible) {
    showNotification(paste("Limiting loss not feasible. Not enough seats to distribute."), type = "error", duration = 2)
  }
  validate(
    need(remainder <= possible, "")
  )
  rep_scen[diffs_rep_scen < 0] <-  rep[diffs_rep_scen < 0]
  if (remainder > 0) {
    for (i in 1:remainder) {
      indicator <- rep_scen - rep_scen_exact
      indicator[diffs_rep_scen <= 0] <-  -10000
      indicator[rep_scen - rep <= 0] <- -10000
      indicator[rep_scen == m] <- -10000
      rep_scen[which.max(indicator)] <- rep_scen[which.max(indicator)] - 1
      # data$rep_scen[which.max(data$rep_scen[data$diffs_rep_scen > 0] - data$rep_scen_exact[data$diffs_rep_scen > 0])] <- data$rep_scen[which.max(data$rep_scen - data$rep_scen_exact)] - 1
      # print(i)
      # print(sum(data$rep_scen))
      # print(which.max(indicator))
    }
  }

  if (min(rep_scen) > m) {
    showNotification(paste("Minimum number of seats not binding. A country would lose too many seats."), type = "error", duration = 2)
  }
  if (max(rep_scen) < M) {
    showNotification(paste("Maximum number of seats not binding. Redistribution from a large country needed to satisfy limited loss."), type = "error", duration = 2)
  }
  return(list(rep_scen = rep_scen, r = remainder))
}
