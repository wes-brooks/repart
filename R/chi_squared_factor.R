#' perform chi-squared test of y vs. x
#' @param x A factor variable to be cross-tabulated with outcome
#' @param outcome A matrix of binned observed categories
#' @param weights A vector of prior weights
#' @return Log p-value of the chi-squared statistic from the contingency table of x vs outcome.
#' 
chi_squared_factor <- function( x, outcome, weights ) {
  
  # fail if x is not a vector
  if ( !is.factor(x) ) stop( "Must use chi_squared_factor with a factor variable x." )
  
  # return NA if x takes only one level among the variables in this node
  if ( length( levels(x) ) < 2 ) return( NA )
  
  # calculate the p-value of a chi-squared test for independence between x and outcome.
  ct <- suppressWarnings( chisq.repart( table.repart( outcome, x, weights ) ) )
  stats::pchisq( ct$statistic, ct$parameter, log = TRUE, lower.tail = FALSE )
}