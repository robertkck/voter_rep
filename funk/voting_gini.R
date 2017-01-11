# Gini function
voting_gini <- function(pop_share, rep_share){
  df <- data.frame(pop = pop_share, rep= rep_share)
  df$pop_rep <- df$rep / df$pop
  df <- df[with(df, order(pop_rep)),]
  df$rep_share_cum <- cumsum(df$rep)
  df$area <- (df$rep_share_cum - df$rep / 2) * df$pop
  gini <- 1 - sum(df$area)/0.5
  return(gini)
}