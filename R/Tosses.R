#' @title Coin Toss
#' @description A generic gunction to simulate the tossing of a coin
#' @param ... additional parameters that may be supplied.
#' @author Justin Strate
#' @export
toss <- function(...){
  UseMethod("toss")
}

#' @title Toss a Coin
#' @description Toss a coin
#' @param mycoin an object of class coin
#' @param n the number of times to toss the coin
#' @param ... additional parameters that may be supplied.
#' @export
toss.coin <- function(mycoin,n, ...){
  prob <- mycoin$prob
  success <- mycoin$success
  failure <- mycoin$failure
  bern_vars <- stats::rbinom(n, size = 1, prob = prob)
  mytrial <- ifelse(bern_vars ==1, success, failure)
  structure(list(success=success, failure=failure, prob=prob, trial=mytrial),
            class=c('tossedCoin', 'coin'))
}

#' @title Coin Toss
#' @description Simulates the tossing of a coin n times.
#' @param n The number of times the coin is tossed
#' @param prob The probability of "Success"; default is 0.5 to simulate a fair coin
#' @param success Denotes "success"; default is "H"
#' @param failure Denotes "failure"; default is "T"
#' @param ... additional parameters that may be supplied.
#' @details Can also be thought of as simulating n bernoulli trials; not just coin tosses but any trial in which the outcome is binary.
#' @author Justin Strate
#' @export
toss.default <- function(n = 1, prob = 0.5, success = 'H', failure = 'T', ...){
  x <- stats::rbinom(n,  size = 1, prob = prob)
  out <- ifelse(x==1, success, failure)
  return(out)
}

