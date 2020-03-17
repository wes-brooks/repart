#' Calculate the classification entropy of a factor
#' 
#' @param bins The outcome variable, represented as a data.frame where each column is an indicator for one level of the factor
#' @return The information entropy of the binned variable
#' @export
entropy_deprecated <- function(bins) {
  if (nrow(bins) == 0) {0}
  else {
    # bins <- next_iter #this should be the assignment indicators for this group, including random effects.
    pp <- colSums(bins) / sum(bins)
    
    sum( ifelse(pp == 0, 0, pp * log2(pp)) )
  }
}