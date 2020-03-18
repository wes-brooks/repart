#' Calculate residuals for a classification tree
#' 
#' @param tree A classification tree fitted by repart
#' @param type The typr of residuals to compute
#' @return A matrix of the classwise residuals
#' @export
residuals.repart <- function( tree, type = "logit" ) {
  
  # get the fitted values
  outcome <- stats::model.matrix( ~ -1 + `(response)`, data = tree$fitted )
  fit <- fitted.repart( tree )
  
  # calculate residuals as observed - fitted
  resid <- outcome - fit
  colnames( resid ) <- colnames( fit )

  # return the residuals
  resid
}