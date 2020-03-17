#' perform chi-squared test of y vs. x
chi_squared <- function( x, outcome, weights ) {
  
  x <- factor( x )
  if ( length( levels(x) ) < 2 ) return( NA )
  ct <- suppressWarnings( chisq.repart( table.repart( outcome, x, weights ), correct = FALSE ) )
  pchisq( ct$statistic, ct$parameter, log = TRUE, lower.tail = FALSE )

  # ## Bonferroni-adjusted p-value small enough?
  # if ( all( is.na(logp) ) ) return( NULL )
  # minp <- exp( min( logp, na.rm = TRUE ) )
  # minp <- 1 - (1 - minp)^sum( !is.na( logp ) )
  # 
  # -minp
}