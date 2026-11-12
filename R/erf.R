#' Error function and its inverse
#'
#' Calculates the error function, its complement and their inverses, used for the normal distribution.
#'
#' This function calculates the value of the error according to the          
#' normally distributed variable using the idea presented in   
#' Abramowitz and Stegun (2011)
#'
#' @param x The data that is provided
#'
#' @return \code{} Gives the error for a normal variable
#'
#' @author Karuna Reddy <karuna.reddy@auckland.ac.nz>\cr
#' MGM Khan <khan_mg@usp.ac.fj>
#'
#' @keywords internal
#the 'error function'
erf <- function(x)
  {
  2*pnorm(x*sqrt(2)) - 1
  }
#the 'complementary error function'
erfc <- function(x)
  {
  2*pnorm(x*sqrt(2), lower.tail = FALSE)
  }
#and the inverses
erfinv <- function(x)
  {
  qnorm((1 + x)/2)/sqrt(2)
  }
erfcinv <- function(x)
  {
  qnorm(x/2, lower.tail = FALSE)/sqrt(2)
}
######################################################################