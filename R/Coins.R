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


#' Convert \code{data.frame} to \code{list}.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param x A \code{data.frame} object.
#' @examples
#' my_result <- foo(iris)
#'
foo <- function(x) {
  x %>%
    as.list()
}


#' @title Print Coin
#' @description prints a coin
#' @author Justin Strate
#' @param x the object to be printed
#' @param ... Other arguments that may be supplied
#' @export
print.coin <- function(x, ...){
   succ <- x$success
   fail <- x$failure
   prob <- x$prob

   mystring <- paste("A Bernoulli trial with success denoted by '", succ,
                     "' failure denoted by '",
                     fail,"', and probability of success ", prob,'.', sep = '')
   print(mystring)
}


#' @title Print Tossed Coin
#' @description prints the trial of the tossed coin
#' @param x the object to be printed
#' @param  ... other arguments that may be supplied
#' @export
print.tossedCoin <- function(x, ...){
  print(x$trial)
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



#' @title Creating a Tossed Coing from a Vector
#' @description Creates a tossed coin from either a characeter or factor vector of two levels
#' @param vec a vector reprsenting the trial
#' @param success denotes "success" in the trial
#' @param failure denotes "failure" in the trial
#' @param prob Represents the probability of "Success". Default is NULL
#' @param ... other arguments that may be supplied
#' @export
tossed_coin <- function(vec, success, failure, prob = NULL, ...)UseMethod("tossed_coin")


#' @title Creating a Tossed Coing from a Vector
#' @description Creates a tossed coin from either a characeter or factor vector of two levels
#' @param vec a character vector reprsenting the trial
#' @param success denotes "success" in the trial
#' @param failure denotes "failure" in the trial
#' @param prob Represents the probability of "Success". Default is NULL
#' @param ... other arguments that may be supplied
#' @export
tossed_coin.character <- function(vec, success, failure, prob =NULL, ...){
  unique_levels <- unique(vec)
  if(any(is.na(unique_levels)))warning("Please note some of the trials have missing values")

  non_missing_levels <- unique_levels[!is.na(unique_levels)]
  if(length(non_missing_levels )>2)stop('There are more than one two levels')

  #define success and failure if they are both missing
  if(missing(success) & missing(failure)){
    success <- sort(non_missing_levels)[1]
    failure <- sort(non_missing_levels)[2]
  } else if(missing(success) & !missing(failure)){

    #make sure failure is in the unique levels
    if(!failure %in% non_missing_levels)warning("failure is not among the trials")
    success <- non_missing_levels[non_missing_levels != failure]

  }else if(!missing(success) & missing(failure)){

    #make sure success is in the unique levels
    if(!success %in% non_missing_levels)warning("success is not among the trials")
    failure <- non_missing_levels[non_missing_levels != success]

  }

  structure(list(success=success, failure=failure, prob=prob, trial=vec),
            class=c('tossedCoin', 'coin'))

}

