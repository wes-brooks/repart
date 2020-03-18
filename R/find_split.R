find_split <- function(response, data, outcome, splitfun, weights, adjust_threshold, alpha = 0.2) {
  ## extract response values from data
  # y <- factor(rep(data[[response]], weights))
  
  # test splits on all the possible variables
  split_utility <- test_splits( splitfun, data, response, outcome, weights )
  split_utility <- adjust_threshold( split_utility )
  
  ## Bonferroni-adjusted p-value small enough?
  if ( all( is.na(split_utility) ) ) return( NULL )
  if( min(split_utility, na.rm = TRUE) > alpha ) return( NULL )
  
  ## for selected variable, search for split minimizing p-value
  xselect <- which(names(data) != response)
  xselect <- xselect[ which.min( split_utility ) ]
  x <- rep( data[[ xselect ]], weights )
  
  ## set up all possible splits in two kid nodes
  if ( is.factor(x) ) {
    lev <- levels( x[ drop = TRUE ] )
    if ( length(lev) == 2 ) {
      splitpoint <- lev[1]
    } else {
      comb <- do.call("c", lapply(1:(length(lev) - 2),
                                  function(x) combn(lev, x, simplify = FALSE)))
      xlogp <- sapply(comb, function(q) splitfun(x %in% q))
      splitpoint <- comb[[ which.min(xlogp) ]]
    }
  }
  
  
  ## split into two groups (setting groups that do not occur to NA)
  splitindex <- !( levels( data[[ xselect ]] ) %in% splitpoint )
  splitindex[ !(levels( data[[ xselect ]] ) %in% lev ) ] <- NA_integer_
  splitindex <- splitindex - min(splitindex, na.rm = TRUE) + 1L
  
  ## return split as partysplit object
  return(
    partysplit(varid = as.integer(xselect),
               index = splitindex,
               info = list(criterion = split_utility) )
  )
}