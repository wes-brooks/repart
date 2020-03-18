#' Calculate residuals for a single node
#' 
#' @param node A node of the classification tree
#' @return A matrix of the classwise residuals
#' @export
node_residuals_deprecated <- function( node ) {
  # calculate the fitted values
  fitted = colMeans( node[[ "outcome" ]] )
  
  # residuals are actual - fitted
  resid <- sweep(node[[ "outcome" ]], 2, fitted, '-')
  
  resid
}