#' Derive crisp assignments for given set of prototypes
#'
#' @param X The N x n design matrix, N observations and n (preferably) scaled features
#' @param W The K x n prototype matrix returned by the neural gas algorithm
#' @param dist_fun The distance or dissimilarity function used.
#'
#' @returns A vector of length N, containing the prototype index for each of the N observations.
#' @export
#'
#' @examples
compute_assignments <- function(X, W, dist_fun){
  apply(X, 1, function(x_i){
    dk <- dist_fun( W = W, x_i = x_i)
    which.min( dk )
  })
}

