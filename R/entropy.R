#' Calculate the classification entropy of a factor
#' 
#' @param bins The outcome variable, represented as a data.frame where each column is an indicator for one level of the factor
#' @return The information entropy of the binned variable
#' @export
entropy <- function ( x, outcome, weights ) {
  if (nrow(outcome) == 0) { 0 }
  else {
    # bins <- next_iter #this should be the assignment indicators for this group, including random effects.
    # pp <- colSums(outcome) / sum(outcome)
    # 
    # sum( ifelse(pp == 0, 0, pp * log2(pp) *) ) * nrow(outcome)

    # begin by calculating the entropy if there is no split on this variable
    cs <- colSums(outcome * weights)
    entr_prior <- sum( ifelse( cs == 0, 0, cs * log2(cs / sum(cs)) ) )
    
    x <- factor( x )
    if ( length( levels(x) ) < 2 ) return( NA )
    counts <- table.repart( outcome, x, weights )

    # calculate the entropy of the proposed split
    entr_prop <- apply(counts, 2, function(col) {
      ifelse( col == 0, 0, col * log2(col / sum(col)) )
    })
    
    # return the improvement in entropy
    entr_prior - sum( entr_prop )
  }
}
