#' Return two new nodes, from splitting the data on a dichotomous variable

#' @param node A node of the tree
#' @param split A list that tells repart which variable to split the node on
#' @return The node, changed from a leaf to a branch
#' @export
do_split <- function(node, split) {
  children <- list()
  df <- node[["data"]]
  sv <- split[["variable"]]
  uv <- unique( df[[sv]] )
  
  # dichotomous - for now.
  for (i in 1:2) {
    indx <- df[[sv]] == uv[[i]]
    df_child <- df[indx, ]
    outcome_child <- node[["outcome"]][indx, ]
    
    # populate the new nodes
    children[[i]] <- list(data = df_child,
                          outcome = outcome_child,
                          entropy = nrow(outcome_child) * entropy(outcome_child),
                          leaf = TRUE,
                          parent = split[[ "node" ]])
  }
  
  # attach some metadata to this node
  parent = list()
  parent[["leaf"]] <- FALSE
  parent[["split"]] <- split
  parent[["entropy"]] <- sum( sapply(children, function(ch) ch[[ "entropy" ]]) )
  parent[[ "data" ]] <- df
  parent[[ "parent" ]] <- node[[ "parent" ]]
  
  # return value
  list(parent, children)
}