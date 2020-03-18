#' A chi-square test that can work with observation weights and offsets
#' @param x A table of possibly non-integer counts (due to random effects)
#' @return A list with elements `statistic` (a chi-squared test statistic) and `parameter` (the number of degrees of freedom for `statistic`)
chisq.repart <- function(x) {
  E <- matrix(NA, nrow=nrow(x), ncol=ncol(x))
  
  # compute the margins of the contingency table
  row_margin <- rowSums(x)
  col_margin <- colSums(x)
  tot <- sum(x)
  
  # Calculate the expected values of the cells, conditional on the margins
  for (i in 1:nrow(x)) for (j in 1:ncol(x)) {
    E[[i, j]] <- row_margin[[i]] / tot * col_margin[[j]]
  }
  
  # return the chi-square statistic and degrees of freedom
  list( statistic = sum((x - E)**2 / E), parameter = (nrow(x) - 1) * (ncol(x) - 1) )
}

