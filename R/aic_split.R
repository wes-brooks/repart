aic_split <- function(delta) {
  # adjust based on the number of splits being considered here
  if ( all( is.na(delta) ) ) return( NULL )
  
  p <- sum( !is.na(delta) )
  
  ifelse( is.na(delta), NA, delta + 2 * p )
}