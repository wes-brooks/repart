#' Try the possible splits at this node
#' @param splitfun The function that calculates the utility of a proposed split
#' @param data The `data.frame` of data to find a split
#' @param response The name of the response variable in `data`
#' @param outcome A matrix of (non-integer) indicators that specifies the value of the response variable in each observational unit
#' @param weights A vector of prior weights
#' @return A named vector giving the utlity of splitting on each variable
test_splits <- function(splitfun, data, response, outcome, weights) {
  
  # compute the utility on each possible split
  xselect <- which( names(data) != response )
  utility <- sapply(xselect, function(i) splitfun( rep( data[[i]], weights ), outcome, weights ), simplify = FALSE )
  names(utility) <- names(data)[ xselect ]
  
  # if there is a threshold adjustment specified then apply it, otherwise return the values from splitfun
  utility
}