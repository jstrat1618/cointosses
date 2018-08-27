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



#' @title Print Coin
#' @description prints a coin
#' @author Justin Strate
#' @param coin a coin object
#' @param ... Other arguments that may be supplied
#' @export
print.coin <- function(x, ...){
   succ <- coin$success
   fail <- coin$failure
   prob <- coin$prob

   mystring <- paste("A Bernoulli trial with success denoted by '", succ,
                     "' failure denoted by '",
                     fail,"', and probability of success ", prob,'.', sep = '')
   print(mystring)
}



#' @title Create An Autocorrelated Coin
#' @description Creates an autocorrelated coin
#' @author Justin Strate
#' @param success Denotes "success" in the Bernoulli Trial. Default is "H".
#' @param failure Denotes "failure" in the Bernoulli Trial. Default is "F".
#' @param initial_prob The initial probability of "success". Must be numeric and be between 0 and 1. Default is 0.5.
#' @param p_given1 The probability of "success" given the previous trial was a "success". Must be numeric and be between 0 and 1. Default is 0.5.
#' @param p_given0 The probability of "success" given the previous trial was a "failure". Must be numeric and be between 0 and 1. Default is 0.5.
#' @export
autoCorrCoin <- function(success = 'H', failure = 'T', initial_prob = 0.5, p_given1 = 0.5,
                       p_given0 = 0.5 ){

  probs <- c(initial_prob, p_given1, p_given0)
  if(!is.numeric(probs))stop("All probabilities should be numeric")

  rngChck <- all(probs >= 0 & probs <= 1)
  if(!rngChck)stop("All probabilities should be between 0 and 1")


  structure(list(success=success, failure=failure, initial_prob=initial_prob,
                 p_given1=p_given1, p_given0=p_given0), class='autoCorrCoin')

}
