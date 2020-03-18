#' Calculate residuals for a classification tree
#' 
#' @param object A classification tree fitted by repart
#' @param type The typr of residuals to compute
#' @param ... Other arguments passed to residuals
#' @return A matrix of the classwise residuals
#' @export
residuals.repart <- function( object, type = "logit", ... ) {
  
  # get the fitted values
  outcome <- stats::model.matrix( ~ -1 + `(response)`, data = object$fitted )
  fit <- fitted.repart( object )
  
  # calculate residuals as observed - fitted
  resid <- outcome - fit
  colnames( resid ) <- colnames( fit )

  # return the residuals
  resid
}