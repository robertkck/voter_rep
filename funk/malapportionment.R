# MAL function
mal <- function(pop, rep){
  pop_share <- pop / sum(pop)
  rep_share <- rep / sum(rep)
  mal <- 0.5 * sum(abs(rep_share - pop_share))
  return(mal)
}
