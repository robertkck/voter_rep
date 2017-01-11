# Parabolic method, allocation
alloc.parabolic <- function(pop, m, M, H ){
  c <- -0.01
  exante <- pop * H / sum(pop)
  m_par <- min(exante)
  M_par <- max(exante)
  rep <- rep(100, length(pop))
  i <- 0
  while (sum(rep)>H) {
    a <- m* (M_par / (M_par-m_par)) - M * m_par / (M_par - m_par) + c *(M_par* (M_par*m_par - m_par^2))/(M_par - m_par)
    b <- (M-m)/(M_par - m_par) - c * (M_par^2 - m_par^2)/(M_par - m_par)
    rep_exact <- a + b * exante + c * exante^2
    rep = round(rep_exact)
    c <- c + 0.00001
    i <- i + 1
  }
  return(list(rep = rep, rep_exact = rep_exact, c = c, i = i))
}