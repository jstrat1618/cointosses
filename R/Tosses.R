toss <- function( n = 1, prob = 0.5, success = 'H', failure = 'T'){
  x <- rbinom(n,  size = 1, prob = prob)
  out <- ifelse(x==1, success, failure)
  return(out)
}