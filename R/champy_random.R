#' @title Random Sample Generator for the Champy Distribution
#'
#' @description
#' Generates random samples from the Champy continuous distribution
#' using the acceptance-rejection method with a constant majorant.
#'
#' @param n An integer specifying the number of random samples to generate.
#'
#' @return A numeric vector of length \code{n} containing random samples
#' drawn from the Champy distribution.
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
#' This function implements the acceptance-rejection algorithm:
#' \enumerate{
#'   \item Generate a candidate \code{x} from a uniform distribution on [-1, 1].
#'   \item Generate a uniform random variable \code{u} in [0, 1].
#'   \item Accept \code{x} if \code{u <= f(x)/m(x)}, where \code{m(x) = 15/16}.
#'   \item Repeat until \code{n} samples are accepted.
#' }
#'
#' @examplesz
#' champy_random(5)
#'
#' @export
champy_random <- function(n) {
  muestra <- numeric(n)
  i <- 1
  while(i <= n) {
    x <- runif(1, -1, 1)   # candidate
    u <- runif(1)          # uniform for acceptance
    if(u <= champy_densidad(x) / (15/16)) {
      muestra[i] <- x
      i <- i + 1
    }
  }
  return(muestra)
}
