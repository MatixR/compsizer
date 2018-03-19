#' Calculates cosine similarity between the columns of a matrix
#'
#' @param x Matrix for which cosine similarities are to be calculated. Currently, the function requires the matrix to be symmetric and free of missing values. The matrix must have named columns specifying the identity of the things across which similarity is measured. (In my application, these are fund WFICNs.)
#' @param lower.tri.only Should the returned matrix include only the lower triangle of similarities? (Useful for efficient storage of large matrices, as the returned matrix of cosine similarities is symmetric.)
#'
#' @return Matrix of cosine similarities, with row and column names of what is being compared.
#'
#' @export
#'
#' @examples
#' x.same <- matrix(0.5, 2, 2)
#' colnames(x.same) <- c("fund1", "fund2")
#' rownames(x.same) <- c("stock1", "stock2")
#' CosSim(x.same)
#' x.orthogonal <- cbind(c(1, 0), c(0, 1))
#' colnames(x.orthogonal) <- c("fund1", "fund2")
#' rownames(x.orthogonal) <- c("stock1", "stock2")
#' CosSim(x.orthogonal)
#' x <- cbind(c(0.75, 0.25), c(0.25, 0.75))
#' colnames(x) <- c("fund1", "fund2")
#' rownames(x) <- c("stock1", "stock2")
#' CosSim(x)

CosSim <- function(x, lower.tri.only = FALSE) {

  # check argument
  if (class(x) != "matrix") {
    stop("Argument must be a matrix")
  }
  if (any(!is.finite(x))) {
    stop("Argument must have no missing values")
  }
  if (is.null(colnames(x))) {
    stop("Please name matrix columns")
  }

  # numerator: dot products between columns
  cos.sim.numerator <- crossprod(x)
  # denominator: product of column magnitudes
  cos.sim.denominator <- tcrossprod(sqrt(colSums(x^2)))
  # cosine similarity
  cos.sim <- cos.sim.numerator / cos.sim.denominator

  # do you want the lower triangle only?
  if (lower.tri.only == TRUE) {
    cos.sim[upper.tri(cos.sim)] <- NA
  }

  return(cos.sim)
}
