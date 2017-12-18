#' @title Streaks
#' @description Counts the streaks
#' @author Justin Strate
#' @param  ... Additional parameters that may be applied
#' @return An integer vector that records the number of streaks of successes
#' @export
streaks <- function(...){
  UseMethod("streaks")
}

#streaks.tossedCoin <- function(mycoin, ...)

#' @title streak
#' @description counts streaks
#' @param x a binary vector of successes and failures
#' @param ... additional parameters
#' @author Justin Strate
#' @return an integer vector that records the number of success
#' @export
streaks.default <- function(x, ...){


  count <- 1
  run <- numeric()

  for(iter in 2:length(x)){
    previous_item <- x[iter-1]
    current_item <- x[iter]

    if(previous_item == current_item){
      count <- count + 1
    } else{
      run <- c(run, count)
      count <- 1
    }
    if(iter == length(x) & count != 1){
      run <- c(run, count)
    }

  }

  run

}

