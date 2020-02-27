#' Calculate the classification entropy of a factor
#' @export
entropy <- function(bins) {
  if (nrow(bins) == 0) {0}
  else {
    # bins <- next_iter #this should be the assignment indicators for this group, including random effects.
    pp <- colSums(bins) / sum(bins)
    sum(pp * log2(pp))
  }
}