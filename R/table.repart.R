#' An analog of the built-in table function, but this one allows for incremental values in the cells to be summed up
#' (a behavior that's necessary for random-effect trees)
#' 
#' @param y The outcome variable - a factor represented as a matrix where each column is a class and each row is an observation
#' @param x The grouping variable
#' @param weights A vector of prior weights
#' @return A contingency table with possibly non-integer counts (due to random effects, the indicators aren't constrained to 1/0, and the weights may be any number)
table.repart <- function(y, x, weights) {
  if (!is.factor(x))
    stop("table.repart only works on factors")
  
  # create an empty matrix where each row is a level of the categorical response
  # and each column is a level of a categorical predictor.
  result <- matrix(NA, nrow = ncol(y), ncol = length( levels(x) ) )
  
  for (i in 1:nrow(result)) for ( j in 1:ncol(result) ) {
    result[[ i, j ]] <- sum( (y * weights)[ x == levels(x)[[ j ]], i ] )
  }
  
  result
}