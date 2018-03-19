#' Calculates CompetitorSize vector for a single date, given similarity and size
#'
#' @param sim.mat Matrix of similarity scores.
#' @param fund.size Mector of fund sizes.
#'
#' @return Matrix of competitor sizes.
#'
#' @export
#'
#' @examples
#' w <- cbind(c(0.75, 0.25), c(0.25, 0.75))
#' colnames(w) <- c("fund1", "fund2")
#' sim.mat <- CosSim(w)
#' fund.size <- matrix(c(0.1, 0.2), 2, 1)
#' rownames(fund.size) <- c("fund1", "fund2")
#' CompSize(sim.mat, fund.size)

CompSize <- function(sim.mat, fund.size) {

  # check inputs
  if (class(sim.mat)   != "matrix") {
    stop("First argument must be a matrix")
    }
  if (class(fund.size) != "matrix") {
    stop("Second argument must be a matrix")
    }

  if (!isSymmetric(sim.mat)) {
    stop("Similarity matrix must be symmetric")
  }
  if (any(!is.finite(sim.mat))) {
    stop("Similarity matrix must have no missing values")
  }

  if (any(!is.finite(fund.size))) {
    stop("Fund size must be non-missing")
  }
  if (any(fund.size < 0)) {
    stop("Fund size must be positive")
  }

  # make sure funds are lined up correctly
  if (!identical(rownames(sim.mat), rownames(fund.size))) {
    stop("Row names do not match")
  }

  # exclude own fund
  diag(sim.mat) <- 0
  # calculate competitor size
  cs <- sim.mat %*% fund.size
  return(cs)
}
