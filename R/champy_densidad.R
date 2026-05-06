#' @title Probability Density Function of the Champy Distribution
#'
#' @description
#' Evaluates the probability density function (PDF) of the Champy
#' continuous distribution at given values of \code{x}.
#'
#' @param x A numeric value or vector where the density function is evaluated.
#'
#' @return A numeric vector with the corresponding density values.
#'
#' @details
#' The Champy distribution is defined on the interval [-1, 1] with
#' probability density function:
#'
#' \deqn{
#' f(x) =
#' \begin{cases}
#' \frac{15}{16}(1 - x^2)^2, & -1 \leq x \leq 1 \\
#' 0, & \text{otherwise}
#' \end{cases}
#' }
#'
#' This function is vectorized and returns 0 for values outside the support.
#'
#' @examples
#' champy_densidad(0)
#' champy_densidad(c(-1, 15/12, 1))
#'
#' @export
champy_densidad <- function(x) {
  ifelse(x >= -1 & x <= 1, (15/16) * (1 - x^2)^2, 0) }
