#' @title Coin Toss
#' @description Simulates the tossing of a coin n times.
#' @param n The number of times the coin is tossed
#' @param prob The probability of "Success"; default is 0.5 to simulate a fair coin
#' @param success Denotes "success"; default is "H"
#' @param failure Denotes "failure"; default is "T"
#' @details Can also be thought of as simulating n bernoulli trials; not just coin tosses but any trial in which the outcome is binary.
#' @author Justin Strate
toss <- function( n = 1, prob = 0.5, success = 'H', failure = 'T'){
  x <- rbinom(n,  size = 1, prob = prob)
  out <- ifelse(x==1, success, failure)
  return(out)
}
