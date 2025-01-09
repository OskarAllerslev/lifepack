#' Equivalence Premium
#' This function calculates the equivalence premium for a given insurance contract
#' This function requires proper construction of reward matrix like specified in the lecture notes provided in the course Liv1 at the University of Copenhagen
#' This function requires proper construction of the derivative of the reward matrix
#' @param Lambda intensity matrix
#' @param s initial timepoint
#' @param t end timepoint
#' @param n number of steps for the runge kutta algorithm
#' @param r constant rate as a scalar
#' @param mu equivalence premium guess
#' @param dR differential of reward matrix
#' @param R reward matrix
#' @return a scalar
#' @export
equiv_premium <- function(s, t, Lambda, R, dR, mu, r, n) {
  b <- reserve(s, t, Lambda, R, 0.0, r, n)
  a <- reserve(s, t, Lambda, dR, 0.0, r, n)
  return(-b / a)
}
