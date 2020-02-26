make_tree <- function(data, outcome) {
  my_tree <- list()
  my_tree[["nodes"]][[1]] <- list(data = data, outcome = outcome, entropy = nrow(outcome) * entropy(outcome), leaf=TRUE)
  my_tree[["metadata"]] <- list(entropy = my_tree[["nodes"]][[1]][["entropy"]], data = data)
  
  # prior_loss is the 
  
  while (TRUE) {
    possible_splits = list()
    
    for (i in 1:length(tree[["nodes"]])) {
      node <- tree[["nodes"]][[i]]
      
      if (node[["metadata"]][["leaf"]]) {
        # calculate the entropy contributed by all other nodes, excluding this one
        outside_entropy <- my_tree[["metadata"]][["entropy"]] - node[["entropy"]]
        
        # test splitting at this node - only returns the variable that maximizes entropy at this node
        possible_splits[[i]] = max(test_entropy(node) + outside_entropy)[[1]]
      }
    }
    
    # do the split that has the biggest increase in entropy. This part is still pseudocodey.
    my_split = which.max(possible_splits)
    if (my_split[["entropy"]] > prior_entropy) {
      tree[[my_split]] = do_split(node, my_split)
      tree[["metadata"]][["entropy"]] <- my_split[["entropy"]]
    } else {
      # stop looping if we haven't improved
      break
    }
    
  }
}