#' Calculate the classification entropy of a factor
#' 
#' @param x The factor variable to consider splitting along
#' @param outcome The outcome variable, represented as a `matrix` where each column is an indicator for one level of the factor
#' @param weights A `vector` of prior weights
#' @return The information entropy of the binned variable
#' @export
entropy <- function ( x, outcome, weights ) {
  if ( length( unique( x ) ) < 2 ) return( NA )
    
  if (nrow(outcome) == 0) return( 0 )

  # begin by calculating the entropy if there is no split on this variable
  cs <- colSums(outcome * weights)
  entr_prior <- sum( ifelse( cs == 0, 0, cs * log2(cs / sum(cs)) ) )
  
  # send the calculation to the right function based on variable type
  if ( is.factor(x) ) entr_prior - entropy_factor( x, outcome, weights )
  else if ( is.numeric( x ) ) {
    split_entropy <- sweep_threshold( x, outcome, weights, entropy_factor )
    result <- entr_prior - split_entropy
    
    # attach the 'split' attribute so we know where to split on this variable
    attr( result, "split") <- attr(split_entropy, "split")
    result
  }
}
