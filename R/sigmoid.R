#' Sigmoid function
#'
#' Implementation of sigmoid function: f(x) = 1 / (1 + exp(-x))
#'
#' @param x
#'
#' @return
sigmoid <- function(x){
  return( 1 / (1 + exp(-x)))
}
