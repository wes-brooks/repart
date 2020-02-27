#' loop over the possible splits within this node. Returns a named list where the name is a variable name and the value is the node's entropy if a split is made on that variable.
#' @export
test_split <- function(node, minbin = 5) {
  df <- node[["data"]]
  vars <- colnames(df)
  resp_obs <- node[["outcome"]]
  node_entropy <- rep(-Inf, length(vars))
  names(node_entropy) <- vars
  
  for (i in 1:length(vars)) {
    vun <- unique(df[[i]])
    
    # since this is treating all unique values as equal, it only works for categorical (unsorted) varaibles
    ent <- sapply(vun, function(my_val) {
      bin_obs = resp_obs[df[[i]] == my_val,];
      
      # if there are fewer datapoints than minbin, don't create the bin
      ifelse(nrow(bin_obs) < minbin, -Inf, nrow(bin_obs) * entropy(bin_obs))
    })
    
    # calculate the overall entropy for each splitting option
    node_entropy[[i]] <- sum(ent)
  }
  
  node_entropy
}