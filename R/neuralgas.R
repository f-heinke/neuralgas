#' Run Neural Gas algorithm
#'
#' @param X An N x n data matrix of N observations with n (preferably scaled) features.
#' @param K Number of prototypes to initialize
#' @param W Optional K x n matrix of prototypes. When provided, the K argument is ignored.
#' @param lambda_init Initial adaption strength in the first iteration.
#' @param lambda_stop Target adaption strength, effective in the final iteration
#' @param learn_rate Constant learn rate. Defaults to 0.1.
#' @param max_iter Number of iterations to run. Defaults to 100 x N.
#' @param dists_fun The funtion to compute K distances / dissimilarities given a data vector and the prototype matrix.
#' @param update_fun The funtion to compute the update for each of the K prototypes.
#' @param verbose Print life signs.
#' @param refresh Refresh life signs printout after certain number of iterations. Defaults to 250 iterations.
#'
#' @returns A list, similar to returned result of the base R kmeans implementation: A matrix of K prototypes, crisp assignment per data point, and number of assignments per prototype.
#' @export
#'
#' @examples
neuralgas <- function(X,
                       K = NULL,
                       W =  NULL,
                       lambda_init = NULL,
                       lambda_stop = 0.001,
                       learn_rate = 0.1,
                       max_iter = nrow(X) * 100,
                       dists_fun = dists_xi_euclidean,
                       update_fun = update_euclidean,
                       verbose = T,
                       refresh = 250){
  
  X <- as.matrix( X )
  N <- nrow(X)
  n <- ncol(X)

  if(is.null(K)){
    K <- ceiling(N / 20)
  }

  if(is.null(W)){
    W <- X[sample(1:nrow(X), K), ]
  }
  if(is.null(lambda_init)){
    lambda_init  <- K / 3
  }

  delta <- exp( log(lambda_stop / lambda_init) / max_iter  )
  lambda <- lambda_init


  for(t in 1:max_iter){
    i <- sample(1:N, 1)
    x_i <- as.matrix(X[i,])

    dists <- dists_fun(W, x_i)
    r <- rank(dists) - 1
    h <- exp(- r / lambda)
    W <- update_fun(x_i = x_i, W = W, learn_rate = learn_rate, h = h)
    lambda <- lambda * delta

    if( verbose ){
      if( t %% refresh == 0){
        print(paste(t, "/",max_iter))
      }
    }
  }

  if( verbose ){
    print(paste(max_iter, "/",max_iter))
  }

  assigns <- compute_assignments( X, W, dists_fun )
  sizes <- sapply(1:K, function(k) sum(k == assigns ) )

  # packing results
  return(
    list(
      prototypes = W,
      assignments = assigns,
      sizes = sizes
    )
  )
}
