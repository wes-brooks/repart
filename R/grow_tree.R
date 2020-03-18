grow_tree <- function(id = 1L, response, data, outcome, weights, splitfun, adjust_threshold, minbucket = 5, ...) { 
  ## for less than 30 observations stop here
  if ( sum(weights) < minbucket ) return( partynode(id = id) )
  
  ## find best split
  sp <- find_split( response, data, outcome, splitfun, weights, adjust_threshold, ... )
  
  ## no split found, stop here
  if ( is.null(sp) ) return( partynode(id = id) ) 
  
  ## actually split the data
  kidids <- kidids_split( sp, data = data ) 
  
  ## set up all daugther nodes
  kids <- vector( mode = "list", length = max( kidids, na.rm = TRUE ) )
  for (kidid in 1:length(kids)) {
    
    ## select observations for current node
    w <- weights
    w[ kidids != kidid ] <- 0
    
    ## get next node id
    if( kidid > 1 ) {
      myid <- max( nodeids( kids[[ kidid - 1 ]] ) )
    } else {
      myid <- id
    }
    
    ## start recursion on this daugther node
    kids[[ kidid ]] <- grow_tree(id = as.integer(myid + 1), response, data, outcome, w, splitfun, adjust_threshold, ...) 
  }
  
  return(
    partynode(
      id = as.integer(id),
      split = sp,
      kids = kids,
      info = list( criterion = min( info_split(sp)$criterion, na.rm=TRUE) )
    ) 
  )
}