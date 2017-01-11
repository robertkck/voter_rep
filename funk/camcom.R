# Cambridge Compromise, Fix + Prop, allocation
alloc.camcom <- function(pop, m, M, H ){
  base <- m-1
  d = 2000000
  inc = 500
  rep <- base + pop / d
  i = 0
  while (sum(rep)<H) {
    rep_exact = pmin(base + pop / d, M)
    rep = ceiling(rep_exact)
    d <- d - inc
    i = i+1
  }
  return(list(rep = rep, rep_exact = rep_exact, d = d, i = i))
}