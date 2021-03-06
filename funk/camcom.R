
#' Cambridge Compromise, Fix + Prop, allocation
#'
#' The function calculates the allocation of MEPs according to the [Cambridge Compromise](http://www.statslab.cam.ac.uk/~grg/papers/cam-report-final2.pdf).
#'
#' @param pop Population count
#' @param m Minimum number of MEPs per country, integer
#' @param M Maximum number of MEPs per country, integer
#' @param H Size of the EP, integer
#'
#' @return The function returns a vector of integers describing the allocation of seats according to the Cambridge Compromise
#' @export
#'
#' @example reps <- alloc.camcom(c(500000, 1000000), 10, 20, 30)

# TODO: Initial value of d and inc as function of pop

alloc.camcom <- function(pop, m, M, H ){
  base <- m-1
  d = 2000000
  inc = 50000
  rep_sum <- 0
  i = 0
  while (rep_sum!=H) {
    rep_exact_nocap = base + pop / d
    rep_exact = pmin(rep_exact_nocap, M)
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
  # print(i)
  return(list(rep = rep, rep_exact = rep_exact, rep_exact_nocap = rep_exact_nocap, d = d, i = i))
}