#' Make a classification tree
#' @export
make_tree <- function(data, outcome) {
  my_tree <- list()
  my_tree[["nodes"]][[1]] <- list(data = data, outcome = outcome, entropy = nrow(outcome) * entropy(outcome), leaf=TRUE)
  my_tree[["metadata"]] <- list(entropy = my_tree[["nodes"]][[1]][["entropy"]], data = data)
  
  # prior_loss is the 
  
  while (TRUE) {
    possible_splits = list()
    split_table <- data.frame(ncol=3)
    
    for (i in 1:length(tree[["nodes"]])) {
      node <- tree[["nodes"]][[i]]
      
      if (node[["metadata"]][["leaf"]]) {
        # calculate the entropy contributed by all other nodes, excluding this one
        outside_entropy <- my_tree[["metadata"]][["entropy"]] - node[["entropy"]]
        
        # test splitting at this node - only returns the variable that maximizes entropy at this node
        test <- test_entropy(node)
        max_index <- which.max(test)[[1]]
        possible_splits[[i]] = max(test) + outside_entropy
        
        # populate the table of splits
        split_table <- rbind(split_table, c(node = i,
                                            variable = names(test)[max_index],
                                            split = 'binary',
                                            entropy = max(test) + outside_entropy))
      } else {
        
      }
    }
    
    # do the split that has the biggest increase in entropy. This part is still pseudocodey.
    splindx = which.max(split_table$entropy)
    my_split <- split_table[splindx, ]
    
    if ( my_split[[ "entropy" ]] > prior_entropy + threshold) {
      tree[[ my_split[[ "node" ]] ]] = do_split( node, my_split )
      tree[[ "metadata" ]][[ "entropy" ]] <- my_split[[ "entropy" ]]
    } else {
      # stop looping if we haven't improved
      break
    }
    
  }
}