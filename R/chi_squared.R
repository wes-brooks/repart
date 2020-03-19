#' Perform chi-squared test of y vs. x
#' 
#' @param x The variable to consider splitting along
#' @param outcome The outcome variable, represented as a `matrix` where each column is an indicator for one level of the factor
#' @param weights A `vector` of prior weights
#' @return The chi-squared test of independence betwen the proposed split variable and the binned variable
#' @export
chi_squared <- function ( x, outcome, weights ) {
  if ( length( unique( x ) ) < 2 ) return( NA )
  
  if (nrow(outcome) == 0) return( 0 )
  
  # send the calculation to the right function based on variable type
  if ( is.factor(x) ) chi_squared_factor( x, outcome, weights )
  else if ( is.numeric( x ) ) sweep_threshold( x, outcome, weights, chi_squared_factor )
}