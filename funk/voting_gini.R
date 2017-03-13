# Gini function
voting_gini <- function(pop, rep){
  df <- data.frame(pop, rep)
  df$pop_share <- df$pop / sum(df$pop)
  df$rep_share <- df$rep / sum(df$rep)
  df$pop_rep <- df$rep_share / df$pop_share
  df <- df[with(df, order(pop_rep)),]
  df$rep_share_cum <- cumsum(df$rep_share)
  df$area <- (df$rep_share_cum - df$rep_share / 2) * df$pop_share
  gini <- 1 - sum(df$area)/0.5
  return(gini)
}