# Create constructor functions
#' @title Create New Coin
#' @description Creates a new coin or Bernoulli Trial
#' @author Justin Strate
#' @param success Denotes "success" in the Bernoulli Trial. Default is "H"
#' @param failure Denotes "failure" in the Bernoulli Trial. Default is "F"
#' @param prob The probability of "success". Must be numeric and be between 0 and 1. Default is 0.5
#' @export
coin <- function(success = 'H', failure = 'T', prob = 0.5){
  if(!is.numeric(prob))stop('prob must be numeric')
  if(prob <0 | prob >1)stop('prob must be between 0 and 1')
  structure(list(success=success, failure=failure, prob=prob), class='coin')
}


#autocorrelatedCoin <- (initial_prob = 0.5, success = 'H', failure = 'T', p_given1 = 0.5,
#                       p_given0 = 0.5 ){



#}
