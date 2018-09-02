#' @title Streaks
#' @description Counts the streaks
#' @author Justin Strate
#' @param  ... Additional parameters that may be applied
#' @return An integer vector that records the number of streaks of successes
#' @export
streaks <- function(...){
  UseMethod("streaks")
}

#' @title streaks.tossedCoin
#' @description coints the streaks for a coin that has been tossed
#' @param mycoin a coin object
#' @param ... Additional parameters that may be applied
#' @export

streaks.tossedCoin <- function(mycoin, ...){
  trial <- mycoin$trial
  success <- mycoin$success
  failure <- mycoin$failure


  streaksSucces <- numeric()
  streaksFailure <- numeric()
  runs <- numeric()

  count <- 1


  for(iter in 2:length(trial)){
    previous_item <- trial[iter-1]
    current_item <- trial[iter]

    if(previous_item == current_item){
      count <- count + 1
    } else if(count != 1){


      runs <- c(runs, count)

      if(previous_item == success){
          streaksSucces <- c(streaksSucces, count)
      } else{
        streaksFailure <- c(streaksFailure, count)

      }

      count <- 1
    }
    if(iter == length(trial) & count != 1){
      runs <- c(runs, count)

      if(previous_item == success){
        streaksSucces <- c(streaksSucces, count)
      } else{
        streaksFailure <- c(streaksFailure, count)

      }

    }

  }

  structure(list(success=success,
                 streaksSucces=streaksSucces,
                 failure=failure,
                 streaksFailure=streaksFailure,
                 runs=runs,
                 trial = trial), class='coinStreaks')

}

#' @title streak
#' @description counts streaks
#' @param x a binary vector of successes and failures
#' @param ... additional parameters
#' @author Justin Strate
#' @return an integer vector that records the number of success
#' @export
streaks.default <- function(x, ...){

  count <- 1
  runs <- numeric()

  for(iter in 2:length(x)){
    previous_item <- x[iter-1]
    current_item <- x[iter]

    if(previous_item == current_item){
      count <- count + 1
    } else if(count != 1){
      runs <- c(runs, count)
      count <- 1
    }
    if(iter == length(x) & count != 1){
      runs <- c(runs, count)
    }

  }

  runs

}

#' @export
sum.tossedCoin <- function(x, ...){


  sum(ifelse(x$trial == x$success, 1, 0), ...)

}

#' @export
mean.tossedCoin <- function(x, ...){

  mean(ifelse(x$trial == x$success, 1, 0), ...)
}

