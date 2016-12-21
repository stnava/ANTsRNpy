#' Simple whitening function.
#'
#' Whitens the input matrix using SVD and returns the result.
#'
#' @param x input matrix
#' @param k rank to use
#' @param reducex reduce the input matrix to k-size subspace
#' @return matrix is output
#' @author Avants BB
#' @examples
#'
#' mat <- matrix(rnorm(300),ncol=50)
#' wmat<-whiten( mat )
#'
#' @export whiten
whiten <- function( x ) {
  return( NA )
}
