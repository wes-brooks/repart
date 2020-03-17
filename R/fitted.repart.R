fitted.repart <- function( tree ) {
  outcome <- model.matrix( ~ -1 + `(response)`, data = tree$fitted )
  fitted <-  matrix( NA, nrow = nrow( outcome ), ncol = ncol( outcome ) )
  colnames( fitted ) <- levels(tree$fitted[[ "(response)" ]])
  
  # loop over the nodes in the tree and get the residuals from each leaf node
  for (node in unique( tree$fitted[[ "(fitted)" ]] ) ) {
    indx <- tree$fitted[[ "(fitted)" ]] == node
    fit <- matrix( rep( colMeans(outcome[indx, ]), each=sum(indx) ), nrow=sum(indx) )
    
    
    fitted[indx,] <- fit
  }
  
  fitted
}