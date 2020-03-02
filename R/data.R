#' Data from an field experiment on beetles
#'
#' A dataset containing the design and results of an experiment that tested the behavior of beetles who encountered spiders.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{Trial}{price in US dollars (\$326--\$18,823)}
#'   \item{Date}{weight of the diamond (0.2--5.01)}
#'   \item{Row}{quality of the cut (Fair, Good, Very Good, Premium, Ideal)}
#'   \item{id}{diamond colour, from D (best) to J (worst)}
#'   \item{spider}{a measurement of how clear the diamond is (I1 (worst), SI2,
#'     SI1, VS2, VS1, VVS2, VVS1, IF (best))}
#'   \item{cue}{length in mm (0--10.74)}
#'   \item{outcome}{width in mm (0--58.9)}
#'   \item{species}{depth in mm (0--31.8)}
#' }
"beetles"