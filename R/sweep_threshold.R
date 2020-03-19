#' For splitting on a continuous variable, sweep the threshold across the possible values
#' 
#' @param x The factor variable to consider splitting along
#' @param outcome The outcome variable, represented as a `data.frame` where each column is an indicator for one level of the factor
#' @param weights A `vector` of prior weights
#' @param factor_function Function to call after `x` has been discretized at each threshold in the sweep
#' @return A named list with elements `threshold`, which is the threshold that maximizes entropy gain and `gain`, which is the maximized entropy gain.
sweep_threshold <- function ( x, outcome, weights, factor_function ) {
  if (nrow(outcome) == 0) { return( 0 ) }
  
  if ( !is.numeric(x) ) stop( "Must call sweep_threshold with a numeric variable" )
    
  # find the possible split values
  valid_splits <- sort( unique(x) )
  trace <- rep( NA, length(valid_splits) )
  
  # loop over the sorted threshold values
  for (i in 1:length(valid_splits) ) {
    
    # discretize x into above/below the threshold
    proposal <- valid_splits[[ i ]]
    indicator <- factor(x <= proposal)
    
    # apply the factor_function to the discretized x
    trace[[ i ]] <- factor_function( indicator, outcome, weights )
  }
  
  # report the threshold that maximizes the entropy gain
  maxi <- which.min( trace )
  result <- trace[[ maxi ]]
  attr( result, "split" ) <- valid_splits[[ maxi ]]
  
  result
}
