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
  if(length(unique_levels)>2)stop('There are more than one two levels')

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

#' @title Toss An Autocorrelated Coin
#' @description Creates an autocorrelated coin
#' @author Justin Strate
#' @param mycoin An Autocorrelated Coin
#' @param n The number of times to toss the autocorrelated coin
#' @param ... additional parameters that may be supplied.
#' @export
toss.autoCorrCoin <- function(mycoin, n, ...){

  p_given1 <- 0.5
  p_given0 <- 0.5
  initial_prob <- mycoin$initial_prob
  success <- mycoin$success
  failure <- mycoin$failure

  trials <- factor(levels = c(success, failure))

  for(iter in 1:n){

    if(iter == 1){
      draw_num <- stats::rbinom(1, size = 1, prob = initial_prob)
      result <- ifelse(draw_num == 1, success, failure)

      trials[iter] <- result
    } else{

      previous_result <- trials[iter - 1]

      if(previous_result == success){
        draw_num <- stats::rbinom(1, size = 1, prob = p_given1)
        result <- ifelse(draw_num == 1, success, failure)

        trials[iter] <- result

      } else{
        draw_num <- stats::rbinom(1, size = 1, prob = p_given0)
        result <- ifelse(draw_num == 1, success, failure)

        trials[iter] <- result
      }

    }

  }

  structure(list(success=success, failure=failure, initial_prob=initial_prob,
                 p_given1=p_given1, p_given0=p_given0, trials=trials),
            class='tossedAutoCorrCoin')
}

