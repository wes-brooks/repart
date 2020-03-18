#' Try the possible splits at this node
test_splits <- function(splitfun, data, response, outcome, weights) {
  
  # compute the utility on each possible split
  xselect <- which( names(data) != response )
  utility <- sapply(xselect, function(i) splitfun( rep( data[[i]], weights ), outcome, weights ) )
  names(utility) <- names(data)[ xselect ]
  
  # if there is a threshold adjustment specified then apply it, otherwise return the values from splitfun
  utility
}