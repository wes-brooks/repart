# return two new nodes, from splitting the data on a dichotomous variable
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
                          leaf = TRUE)
  }
  
  # attach some metadata to this node
  node[["leaf"]] <- FALSE
  node[["split"]] <- split
  node[["children"]] <- children
  node[["entropy"]] <- sum( sapply(children, function(ch) ch[["entropy"]]) )
  
  # return value
  node
}