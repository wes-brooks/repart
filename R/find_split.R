#' Identify which variable to split on, and where
#'
#' @param response Name of the response variable
#' @param data The `data.frame` of data for this tree
#' @param outcome A `matrix` of possibly non-integer indicators for the outcome factor
#' @param splitfun The function that identifies the splits in the tree
#' @param weights A vector of prior weights
#' @param adjust_threshold A function that adjusts the outputs of `splitfun`
#' @param alpha Threshold for deciding whether to split
#' @return `partysplit` for splitting at this node, or `NULL` if no split meets the criteria
find_split <- function(response, data, outcome, splitfun, weights, adjust_threshold, alpha = 0.2) {

  # test splits on all the possible variables
  split_utility <- test_splits( splitfun, data, response, outcome, weights )
  split_utility <- adjust_threshold( split_utility )
  
  ## Bonferroni-adjusted p-value small enough?
  if ( all( is.na(split_utility) ) ) return( NULL )
  if( min( unlist(split_utility), na.rm = TRUE) > alpha ) return( NULL )
  
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
                                  function(x) utils::combn(lev, x, simplify = FALSE)))
      xlogp <- sapply(comb, function(q) splitfun(x %in% q))
      splitpoint <- comb[[ which.min(xlogp) ]]
    }
    
    ## split into two groups (setting groups that do not occur to NA)
    splitindex <- !( levels( data[[ xselect ]] ) %in% splitpoint )
    splitindex[ !(levels( data[[ xselect ]] ) %in% lev ) ] <- NA_integer_
    splitindex <- splitindex - min(splitindex, na.rm = TRUE) + 1L
    
    ## return split as partysplit object
    return(
      partykit::partysplit(varid = as.integer(xselect),
                           index = splitindex,
                           info = list(criterion = split_utility) )
    )
    
  } else if ( is.numeric(x) ) {
    # locate the optimal split
    mindx <- which.min( split_utility )
    splitpoint <- attr( split_utility[[ mindx ]], "split" )
    
    # split the dat into two groups
    ## return split as partysplit object
    return(
      partykit::partysplit(varid = as.integer(xselect),
                           breaks = splitpoint,
                           info = list(criterion = split_utility) )
    )
  }
  
  

}