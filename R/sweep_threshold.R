#' For splitting on a continuous variable, sweep the threshold across the possible values
#' 
#' @param x The factor variable to consider splitting along
#' @param outcome The outcome variable, represented as a `data.frame` where each column is an indicator for one level of the factor
#' @param weights A `vector` of prior weights
#' @return A named list with elements `threshold`, which is the threshold that maximizes entropy gain and `gain`, which is the maximized entropy gain.
#' @export
sweep_threshold <- function ( x, outcome, weights ) {
  if (nrow(outcome) == 0) { 0 }
  else {
    
    if ( is.numeric(x) ) {
      
      # begin by calculating the entropy if there is no split on this variable
      cs <- colSums(outcome * weights)
      entr_prior <- sum( ifelse( cs == 0, 0, cs * log2(cs / sum(cs)) ) )
      
      # find the possible split values
      valid_splits <- sort( unique(x[ weights > 0 ]) )
      trace <- rep( NA, length(valid_splits) )
      
      # loop over the sorted threshold values
      for (i in 1:length(valid_splits) ) {
        
        proposal <- valid_splits[[ i ]]
        indicator <- (x <= proposal)
        
        # 
        x <- factor( indicator )
        if ( length( levels(x) ) < 2 ) return( NA )
        counts <- table.repart( outcome, x, weights )
        
        # calculate the entropy of the proposed split
        entr_prop <- apply(counts, 2, function(col) {
          ifelse( col == 0, 0, col * log2(col / sum(col)) )
        })
        
        # return the improvement in entropy
        trace[[ i ]] <- entr_prior - sum( entr_prop )
      }
      
      # report the threshold that maximizes the entropy gain
      maxi <- which.min( trace, na.rm = TRUE )
      list(threshold = valid_splits[[ maxi ]], gain = trace[[ maxi ]])
    }
    
    
  }
}
