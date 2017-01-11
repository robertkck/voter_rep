# MAL function
mal <- function(pop_share, rep_share){
  mal <- 0.5 * sum(abs(rep_share - pop_share))
  return(mal)
}