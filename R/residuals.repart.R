#' Calculate residuals for a classification tree
#' 
#' @param node A classification tree
#' @return A matrix of the classwise residuals
#' @export
residuals.repart <- function( tree, type = "logit" ) {
  outcome <- model.matrix( ~ -1 + `(response)`, data = tree$fitted )
  fit <- fitted.tree( tree )
  
  resid <- outcome - fit
  colnames( resid ) <- colnames( fit )

  resid
}