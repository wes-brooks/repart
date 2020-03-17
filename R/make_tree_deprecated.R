#' Make a classification tree
#' 
#' @param data The data frame containing all the input variables
#' @param outcome The known outcomes of the tree
#' @return A classification tree to predict the distribution of the outcome based on the supplied data
make_tree_deprecated <- function(data, outcome, threshold = 2) {
  my_tree <- list()
  my_tree[[ "nodes" ]][[ 1L ]] <- list(data = data, outcome = outcome, entropy = nrow(outcome) * entropy(outcome), leaf=TRUE)
  my_tree[[ "metadata" ]] <- list(entropy = my_tree[[ "nodes" ]][[ 1 ]][[ "entropy" ]], data = data, outcome = outcome)
  my_tree[[ "size" ]] <- 1L
    
  while ( TRUE ) {
    # possible_splits = list()
    split_table <- data.frame()
    
    for ( i in 1:length( my_tree[[ "nodes" ]] ) ) {
      node <- my_tree[[ "nodes" ]][[ i ]]
      
      if ( node[[ "leaf" ]] ) {
        # calculate the entropy contributed by all other nodes, excluding this one
        outside_entropy <- my_tree[[ "metadata" ]][[ "entropy" ]] - node[[ "entropy" ]]
        
        # test splitting at this node - only returns the variable that maximizes entropy at this node
        test <- test_split( node )
        max_index <- which.max( test )[[ 1L ]]
        # possible_splits[[ i ]] <- max( test ) + outside_entropy
        
        # populate the table of splits
        split_table <- rbind(split_table, data.frame(node = i,
                                            variable = names( test )[ max_index ],
                                            split = 'binary',
                                            entropy = max( test ) + outside_entropy,
                                            stringsAsFactors = FALSE)
                             )
      } else {
        
      }
    }
    
    # do the split that has the biggest increase in entropy. This part is still pseudocodey.
    splindx = which.max(split_table$entropy)
    my_split <- split_table[splindx, ]
    
    if ( my_split[[ "entropy" ]] > my_tree[[ "metadata" ]][[ "entropy" ]] + threshold) {
      # my_tree[[ "nodes" ]][[ my_split[[ "node" ]] ]] = do_split( node, my_split )
      my_node <- my_tree[[ "nodes" ]][[ my_split[[ "node" ]] ]]
      new_split <- do_split( my_node, my_split )
      my_tree[[ "nodes" ]][[ my_split[[ "node" ]] ]] <- new_split[[ 1L ]]
      my_tree[[ "nodes" ]][[ my_split[[ "node" ]] ]][[ "children" ]] <- 
        my_tree[[ "size" ]] + 1:length( new_split[[ 2L ]] )
      
      my_tree[[ "nodes" ]] = c( my_tree[[ "nodes" ]], new_split[[ 2L ]] )
      
      my_tree[[ "size" ]] <- my_tree[[ "size" ]] + length( new_split[[ 2L ]] )
      my_tree[[ "metadata" ]][[ "entropy" ]] <- my_split[[ "entropy" ]]
    } else {
      # stop looping if we haven't improved
      break
    }
    
  }
  
  my_tree
}