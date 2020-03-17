dunn_sidak <- function(logp) {
  ## Dunn-Sidak-adjusted p-value small enough?
  if ( all( is.na(logp) ) ) return( NULL )
  
  pow <- sum( !is.na(logp) )
  
  ifelse( is.na(logp), NA, 1 - (1 - exp(logp))**pow )
}