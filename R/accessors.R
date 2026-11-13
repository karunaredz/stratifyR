# =============================================================================
# accessors.R  -  stratifyR 2.1
#
# Functions to get results out of "strata" and "compare_designs" objects,
# so that users do not need to know how the objects are built.
# =============================================================================

#' Get the results of a stratification
#'
#' These functions return the main results of \code{\link{strata.data}},
#' \code{\link{strata.distr}} and \code{\link{compare_designs}}, so that the
#' internal structure of the returned objects need not be used.
#'
#' \code{boundaries()} returns the optimum strata boundaries (OSB): the
#' \eqn{L - 1} cut points between the \eqn{L} strata, or, with
#' \code{ends = TRUE}, the \eqn{L + 1} values from the smallest to the
#' largest value of the variable.
#'
#' \code{allocation()} returns one row per stratum, with the lower and upper
#' boundary, the population size \eqn{N_h}, the weight \eqn{W_h = N_h/N}, the
#' standard deviation \eqn{S_h}, the product \eqn{W_h S_h}, the sample size
#' \eqn{n_h} and the sampling fraction \eqn{f_h = n_h/N_h}.
#' \code{as.data.frame()} on a \code{"stratifyR"} object gives the same table.
#'
#' \code{objective()} returns the value of the objective function
#' \eqn{\Phi = \sum_h W_h S_h} at the boundaries, the quantity that the
#' solvers minimise.
#'
#' \code{variance()} returns the variance of the estimated population mean
#' under the design, with the finite population correction:
#' \eqn{V = \sum_h W_h^2 S_h^2 (1/n_h - 1/N_h)}, where a stratum that is
#' sampled completely adds nothing.
#'
#' \code{fitted_distribution()} returns the name of the best-fit distribution
#' and its parameter estimates on the scale of the data.
#'
#' For a \code{"compare_designs"} object, \code{as.data.frame()} returns one
#' row per design (SRS, proportional and Neyman allocation), with the
#' variance and standard error of the sample mean and the design effect, and
#' \code{objective()} returns \eqn{\Phi}.
#'
#' @param x An object of class \code{"stratifyR"}, or for \code{objective()} and
#'   \code{as.data.frame()} also of class \code{"compare_designs"}.
#' @param ends Logical. If \code{TRUE}, add the smallest and largest values
#'   of the variable to the boundaries. Default \code{FALSE}.
#' @param row.names,optional Not used; for compatibility with the generic.
#' @param ... Not used.
#' @return \code{boundaries()}, \code{objective()} and \code{variance()}
#'   return numeric values,
#'   \code{allocation()} and \code{as.data.frame()} return data frames, and
#'   \code{fitted_distribution()} returns a list with components
#'   \code{distr} and \code{estimate}.
#' @seealso \code{\link{strata.data}}, \code{\link{summary.stratifyR}},
#'   \code{\link{compare_designs}}
#' @examples
#' \donttest{
#' data(sugarcane)
#' res <- strata.data(sugarcane$Production, h = 4, n = 500, method = "exact")
#' boundaries(res)
#' allocation(res)
#' objective(res)
#' variance(res)
#' fitted_distribution(res)
#' as.data.frame(compare_designs(res))
#' }
#' @name accessors
NULL

#' @rdname accessors
#' @export
boundaries <- function(x, ...) UseMethod("boundaries")

#' @rdname accessors
#' @export
boundaries.stratifyR <- function(x, ends = FALSE, ...) {
  .check_flag(ends, "ends")
  L   <- nrow(x$h)
  osb <- as.numeric(x$OSB)[seq_len(L)]
  cuts <- osb[-L]
  if (!ends) return(cuts)
  c(x$maxval * x$initval, cuts, x$maxval * x$finval)
}

#' @rdname accessors
#' @export
allocation <- function(x, ...) UseMethod("allocation")

#' @rdname accessors
#' @export
allocation.stratifyR <- function(x, ...) {
  b <- boundaries(x, ends = TRUE)
  L <- nrow(x$h)
  # unrounded values (stored from 2.1-0); rounded ones for older objects
  s  <- if (!is.null(x$stratum_full)) x$stratum_full else
          data.frame(Wh = x$Wh, Vh = x$Vh, WhSh = x$WhSh)
  Nh <- as.numeric(x$Nh); nh <- as.numeric(x$nh)
  data.frame(stratum = seq_len(L),
             lower   = b[-(L + 1L)],
             upper   = b[-1L],
             Nh      = Nh,
             Wh      = as.numeric(s$Wh),
             Sh      = sqrt(as.numeric(s$Vh)),
             WhSh    = as.numeric(s$WhSh),
             nh      = nh,
             fh      = ifelse(Nh > 0, nh / Nh, 0))
}

#' @rdname accessors
#' @export
as.data.frame.stratifyR <- function(x, row.names = NULL, optional = FALSE, ...) {
  allocation(x)
}

#' @rdname accessors
#' @export
objective <- function(x, ...) UseMethod("objective")

#' @rdname accessors
#' @export
objective.stratifyR <- function(x, ...) {
  as.numeric(if (!is.null(x$objective_full)) x$objective_full else x$WhShTot)
}

#' @rdname accessors
#' @export
objective.compare_designs <- function(x, ...) as.numeric(x$WhShTot)

#' @rdname accessors
#' @export
variance <- function(x, ...) UseMethod("variance")

#' @rdname accessors
#' @export
variance.stratifyR <- function(x, ...) {
  if (!is.null(x$variance)) return(as.numeric(x$variance))
  a <- allocation(x)
  s <- a$nh > 0 & a$nh < a$Nh
  sum((a$Wh^2 * a$Sh^2 * (1 / a$nh - 1 / a$Nh))[s])
}

#' @rdname accessors
#' @export
fitted_distribution <- function(x, ...) UseMethod("fitted_distribution")

#' @rdname accessors
#' @export
fitted_distribution.stratifyR <- function(x, ...) {
  est <- tryCatch(if (!is.null(x$fit$estimate)) x$fit$estimate else x$fit$fit$estimate,
                  error = function(e) NULL)
  list(distr = x$distr, estimate = unlist(est))
}

#' @rdname accessors
#' @export
as.data.frame.compare_designs <- function(x, row.names = NULL, optional = FALSE, ...) {
  data.frame(design   = c("SRS", "Proportional", "Neyman"),
             n        = x$n,
             variance = c(x$V_srs, x$V_prop, x$V_opt),
             se       = c(x$SE_srs, x$SE_prop, x$SE_opt),
             deff     = c(1, x$deff_prop, x$deff_opt))
}
