#' Make a classification tree
#' 
#' @param formula A `formula` describing the function to be estimated by the table
#' @param data A `data.frame` containing the data toi be used in fitting the model
#' @param outcome A `matrix` of binned outcomes for training the tree
#' @param splitfun The function to use in assessing each split
#' @param weights A `vector` of prior weights
#' @param adjust_threshold A function used to adjust the output of `splitfun` before assessing possible splits
#' @param ... Other parameters, such as alpha: threshold for deciding whether to split
#' @return A classification tree to predict the distribution of the outcome based on the supplied data
#' @export
#' 
make_tree <- function(formula, data, outcome, splitfun, weights = NULL, adjust_threshold = identity, ...) { 
  ## name of the response variable
  response <- all.vars(formula)[1]
  
  ## data without missing values, response comes last
  data <- data[ stats::complete.cases(data), c( all.vars( formula )[-1], response ) ]
  
  if (is.null(weights)) weights <- rep(1L, nrow(data))
  
  ## weights are case weights, i.e., integers
  stopifnot(length(weights) == nrow(data) &
              max( abs( weights - floor( weights ) ) ) < .Machine$double.eps)
  
  ## grow tree
  nodes <- grow_tree(id = 1L, response, data, outcome, weights, splitfun, adjust_threshold, ...)
  
  ## compute terminal node number for each observation 
  fitted <- partykit::fitted_node(nodes, data = data)
  
  ## return rich constparty object
  ret <- partykit::party(nodes,
               data = data,
               fitted = data.frame(
                 "(fitted)" = fitted,
                 "(response)" = data[[ response ]],
                 "(weights)" = weights,
                 check.names = FALSE
               ),
               terms = stats::terms(formula)
  )
  
  # add the 'repart' S3 class name attribute to the result
  ret <- partykit::as.constparty(ret)
  class(ret) <- c("repart", class(ret))
  
  ret
}