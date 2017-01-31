# Cambridge Compromise, Fix + Prop, allocation
alloc.camcom <- function(pop, m, M, H ){
  base <- m-1
  d = 2000000
  inc = 500
  rep_sum <- 700
  i = 0
  while (rep_sum!=H) {
    rep_exact = pmin(base + pop / d, M)
    rep = ceiling(rep_exact)
    rep_sum <- sum(rep)
    if (rep_sum<H){
      d <- d - inc
      i = i+1
    } else if (rep_sum > H) {
      inc = inc / 2
      d <- d + inc
    }
  }
  return(list(rep = rep, rep_exact = rep_exact, d = d, i = i))
}