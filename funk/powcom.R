# Power Compromise, Fix + Prop^t, allocation
alloc.powcom <- function(pop, m, M, H ){
  out <- alloc.camcom(pop, m, M, H)
  if (ceiling(max(out$rep_exact_nocap))>M) {
    min_rep <- 0
    x <- m - 1
    while (min_rep != m) {
      t <- 1
      inc = 0.1
      i = 0
      max_rep <- 0
        while (max_rep != M & (t > 0.5)) {
          out <- alloc.camcom(pop^t, x, M, H)
          max_rep <- ceiling(max(out$rep_exact_nocap))
          if (max_rep > M){
            t <- t - inc
            i = i + 1
          } else if (max_rep < M) {
            inc = inc / 2
            t <- t + inc
          }
          # t <- t - 0.001
          # i <- i + 1
          # print(paste("T", t, "iteration", i, "Parliament", sum(out$rep), "Max", max_rep))
        }
      min_rep <- min(out$rep)
      print(paste(min_rep, x))
      if (min_rep < m) {x <- x + 1}
      if (min_rep > m) {x <- x - 1}
    }
  }
  # print(paste("PowCom iterations ", i, "t", t))
  return(list(rep = out$rep, rep_exact = out$rep_exact, rep_exact_nocap = out$rep_exact_nocap , d = out$d, i = out$i))
}