#' Compute euclidean distances between some data vector x_k and prototypes
#'
#' @param W The K x n prototype matrix
#' @param x_k The (scaled) k-th data vector
#'
#' @returns K-dimensional vector containing euclidean distances.
#' @export
#'
#' @examples
dists_xi_euclidean <- function(W, x_i){
  #diffs <-  t(t(W) - x_k)
  diffs <- sweep(W,MARGIN = 2, STATS = x_i, FUN = "-")
  dists <- rowSums(diffs^2)
  sqrt( dists )
}

#' The Neural Gas update rule for euclidean vector space-embedded data and prototypes
#'
#' @param x_i A (prefereably scaled) n-dimensional data vector
#' @param W The K x n prototype matrix
#' @param learn_rate Learn rate of the algorithm.
#' @param h The neighborhood-based adaption strength, given the dissimilarity ranks of the prototypes.
#'
#' @returns The K x n  matrix of the K prototypes.
#' @export
#'
#' @examples
update_euclidean <- function(x_i, W, learn_rate, h){

  W <- t(sapply(1:nrow(W), function(j){
    w_j <- W[j,]
    return(w_j + learn_rate * h[j] * (x_i - w_j) ) # Apply the NG adaption scheme to  w_j
  }))
  return( W )
}

