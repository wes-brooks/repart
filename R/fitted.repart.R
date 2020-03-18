#' Extract the fitted values from the tree model
#' 
#' @param tree The tree model from which to extract the fitted values
#' @return The fitted values for the observations used to estimate the tree model
#' @export
fitted.repart <- function( tree ) {
  
  # recover the outcome factor from the input data
  outcome <- stats::model.matrix( ~ -1 + `(response)`, data = tree$fitted )
  fitted <-  matrix( NA, nrow = nrow( outcome ), ncol = ncol( outcome ) )
  colnames( fitted ) <- levels(tree$fitted[[ "(response)" ]])
  
  # loop over the nodes in the tree and get the residuals from each leaf node
  for (node in unique( tree$fitted[[ "(fitted)" ]] ) ) {
    indx <- tree$fitted[[ "(fitted)" ]] == node
    fit <- matrix( rep( colMeans(outcome[indx, ]), each=sum(indx) ), nrow=sum(indx) )
    
    # fill in the fitted values for the observations that ended up in this node
    fitted[indx,] <- fit
  }
  
  # return the fitted outcomes
  fitted
}