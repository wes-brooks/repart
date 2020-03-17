#' Calculate residuals for a classification tree
#' 
#' @param node A classification tree
#' @return A matrix of the classwise residuals
#' @export
residuals.repart_deprecated <- function( tree, type = "logit" ) {
  resid <- matrix(nrow = 0, ncol = ncol( tree[[ "metadata" ]][[ "outcome" ]] ) )
  
  # loop over the nodes in the tree and get the residuals from each leaf node
  for (node in tree[[ "nodes" ]]) {
    if ( node[[ "leaf" ]] ) {
      resid <- rbind( resid, node_residuals( node ) )
    }
  }
  
  resid[ order( as.integer( rownames( resid ) ) ), ]
}