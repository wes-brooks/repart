#' Calculate the classification entropy of a factor
#' 
#' @param x The factor variable to consider splitting along
#' @param outcome The outcome variable, represented as a `data.frame` where each column is an indicator for one level of the factor
#' @param weights A `vector` of prior weights
#' @return The information entropy of the binned variable
#' @export
entropy_factor <- function ( x, outcome, weights ) {

  # prepare the contingency table
  if ( length( levels(x) ) < 2 ) return( NA )
  counts <- table.repart( outcome, x, weights )

  # calculate the entropy of the proposed split
  entr_prop <- apply(counts, 2, function(col) {
    ifelse( col == 0, 0, col * log2(col / sum(col)) )
  })
  
  # return the sum of binned entropies
  sum( entr_prop )
}
