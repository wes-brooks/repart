#' Calculate residuals for a classification tree
#' 
#' @param node A classification tree fitted by repart
#' @return A matrix of the classwise residuals
#' @export
residuals.repart <- function( tree, type = "logit" ) {
  
  # get the fitted values
  outcome <- model.matrix( ~ -1 + `(response)`, data = tree$fitted )
  fit <- fitted.tree( tree )
  
  # calculate residuals as observed - fitted
  resid <- outcome - fit
  colnames( resid ) <- colnames( fit )

  # return the residuals
  resid
}