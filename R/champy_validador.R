#' @title Goodness-of-Fit Test for Champy Distribution
#'
#' @description
#' Performs a Kolmogorov-Smirnov (KS) goodness-of-fit test to validate
#' whether a sample comes from the Champy distribution by comparing
#' the empirical cumulative distribution function (CDF) with the
#' theoretical CDF.
#'
#' @param muestra A numeric vector containing random samples generated
#' from the Champy distribution.
#'
#' @return An object of class \code{"htest"} containing the results of
#' the Kolmogorov-Smirnov test, including the test statistic and p-value.
#'
#' @details
#' The KS test compares the empirical CDF of the sample with the
#' theoretical CDF defined by \code{champy_cdf}. A high p-value indicates
#' that the sample is consistent with the Champy distribution, while a
#' low p-value suggests a significant deviation.
#'
#' @examples
#' muestra <- champy_random(100)
#' champy_validador(muestra)
#'
#' @export
champy_validador <- function(muestra) {
  ks <- ks.test(muestra, champy_cdf)
  return(ks)
}
