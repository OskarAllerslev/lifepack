#' Reserve
#' This function calculates the reserve given the reward matrix and some constant rate
#' This function requires proper construction of reward matrix like specified in the lecture notes provided in the course Liv1 at the University of Copenhagen
#' @param Lambda intensity matrix
#' @param s initial timepoint
#' @param t end timepoint
#' @param n number of steps for the runge kutta algorithm
#' @param r constant rate as a scalar
#' @param mu equivalence premium
#' @param R reward matrix
#' @return returns an entry in a matrix
#' @examples
#' Lambda <- function(x) matrix(c(-0.1, 0.1, 0, -0.1), 2, 2)
#' R <- function(x, mu) matrix(c(0, 0, 0, mu), 2, 2)
#' reserve(0, 80, Lambda, R, 200000, 0.01, 1000)
#' @export
reserve <- function(s, t, Lambda, R, mu, r, n) {
  dim <- nrow(Lambda(t))
  A11 <- function(x) {
    return(Lambda(x) - r * diag(1, nrow = dim))
  }
  RM <- function(x) {
    cbind(rbind(A11(x), matrix(0, dim, dim)), rbind(R(x, mu), Lambda(x)))
  }
  PRM <- prodint(RM, s, t, n)
  RES <- PRM[1:dim, (dim + 1):(2 * dim)]
  return((RES %*% rep(1, dim))[1])
}
