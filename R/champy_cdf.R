#' @title Theoretical Cumulative Distribution Function of the Champy Distribution
#'
#' @description
#' Evaluates the theoretical cumulative distribution function (CDF) of the
#' Champy continuous distribution at given values of \code{x}.
#'
#' @param x A numeric value or vector where the cumulative distribution
#' function is evaluated.
#'
#' @return A numeric vector with the corresponding CDF values.
#'
#' @details
#' The Champy distribution has probability density function:
#'
#' \deqn{
#' f(x) =
#' \begin{cases}
#' \frac{15}{16}(1 - x^2)^2, & -1 \leq x \leq 1 \\
#' 0, & \text{otherwise}
#' \end{cases}
#' }
#'
#' The cumulative distribution function (CDF) is defined as:
#'
#' \deqn{
#' F(x) =
#' \begin{cases}
#' 0, & x \leq -1 \\
#' \int_{-1}^{x} f(t)\, dt, & -1 < x < 1 \\
#' 1, & x \geq 1
#' \end{cases}
#' }
#'
#' This function is vectorized and returns 0 for values below -1 and 1
#' for values above 1.
#'
#' @examples
#' champy_cdf(-1)   # 1
#' champy_cdf(0)    # ~15/16
#' champy_cdf(1)    # 1
#'
#' @export
champy_cdf <- function(x) {
  sapply(x, function(xi) {
    if(xi <= -1) return(0)
    if(xi >= 1) return(1)
    integrate(function(t) champy_densidad(t), -1, xi)$value
  })
}
