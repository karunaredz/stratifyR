# =============================================================================
# compare_designs.R  -  stratifyR 2.0
#
# Compares three survey design efficiencies for the same stratification result:
#   1. Simple Random Sampling Without Replacement (SRSWOR)
#   2. Stratified Sampling with Proportional Allocation
#   3. Stratified Sampling with Neyman (Optimal) Allocation
#
# Reference:
#   Cochran, W.G. (1977). Sampling Techniques, 3rd ed. Wiley. sec 5.5-5.6
#   Kish, L. (1965). Survey Sampling. Wiley.  (design effect definition)
#   Neyman, J. (1934). JRSS 97, 558-625.
# =============================================================================

#' Compare Survey Design Efficiencies
#'
#' Given a fitted \code{"strata"} object (from \code{\link{strata.data}} or
#' \code{\link{strata.distr}}), computes and compares the variance of the
#' sample mean under three designs for the \strong{same} total sample size
#' \eqn{n}:
#'
#' \describe{
#'   \item{SRS}{Simple random sampling without replacement (baseline).}
#'   \item{Proportional}{Stratified sampling with \eqn{n_h \propto W_h}
#'     (proportional allocation).}
#'   \item{Neyman}{Stratified sampling with \eqn{n_h \propto W_h S_h}
#'     (optimal/Neyman allocation) - the allocation used by \code{stratifyR}.}
#' }
#'
#' Design effects (DEFF) follow Kish (1965): \eqn{\mathrm{DEFF} = V_{\rm design}
#' / V_{\rm SRS}}.  The \emph{equivalent SRS sample size} is the number of
#' observations an SRS design would need to match the precision of the Neyman
#' design, and the \emph{cost saving} is the corresponding percentage reduction.
#'
#' @param object An object of class \code{"strata"}.
#' @param n      Integer.  Total sample size.  Defaults to \code{object$nhTot}
#'   (the value used when the stratification was run).
#' @param ...    Currently unused.
#'
#' @return An object of class \code{"compare_designs"} (an invisibly-printed
#'   list) with components:
#'   \describe{
#'     \item{\code{n}}{Total sample size used.}
#'     \item{\code{H}}{Number of strata.}
#'     \item{\code{S2}}{Estimated population variance \eqn{S^2}.}
#'     \item{\code{V_within}}{Within-stratum variance component
#'       \eqn{\sum W_h S_h^2}.}
#'     \item{\code{V_srs, V_prop, V_opt}}{Variance of \eqn{\bar{y}} under
#'       SRS, proportional, and Neyman designs.}
#'     \item{\code{SE_srs, SE_prop, SE_opt}}{Corresponding standard errors.}
#'     \item{\code{deff_prop, deff_opt}}{Design effects relative to SRS.}
#'     \item{\code{n_srs_equiv}}{Equivalent SRS sample size for same precision
#'       as Neyman design.}
#'     \item{\code{pct_saving}}{Percentage sample-size saving of Neyman over
#'       SRS.}
#'     \item{\code{WhShTot}}{\eqn{\sum W_h S_h} (objective function value).}
#'   }
#'
#' @examples
#' \dontrun{
#' res <- strata.data(data = anaemia$Iron, h = 3, n = 300)
#' cd  <- compare_designs(res)
#' cd
#' }
#'
#' @seealso \code{\link{strata.data}}, \code{\link{strata.distr}}
#'
#' @export
compare_designs <- function(object, ...) UseMethod("compare_designs")

#' @rdname compare_designs
#' @export
compare_designs.strata <- function(object, n = NULL, ...) {

  if (!inherits(object, "strata"))
    stop("'object' must be of class \"strata\".")

  n_total <- if (!is.null(n)) as.integer(n) else object$nhTot
  if (n_total < 1L) stop("'n' must be a positive integer.")

  H   <- nrow(object$h)
  Wh  <- object$Wh
  Vh  <- object$Vh            # S_h^2 per stratum
  WSh <- object$WhShTot       # sumW_h S_h  (stratifyR objective)

  # Within-stratum variance: sum W_h S_h^2  (used for proportional allocation)
  V_within <- sum(Wh * Vh)

  # Population variance S^2
  S2 <- .S2_from_strata(object)

  # -- Variances of ybar for each design (at common n) ------------------------
  V_srs  <- S2       / n_total            # SRSWOR
  V_prop <- V_within / n_total            # proportional allocation
  V_opt  <- WSh^2    / n_total            # Neyman (optimal) allocation

  # Standard errors
  SE_srs  <- sqrt(max(V_srs,  0))
  SE_prop <- sqrt(max(V_prop, 0))
  SE_opt  <- sqrt(max(V_opt,  0))

  # -- Design effects relative to SRS (Kish 1965): DEFF = V / V_SRS --------
  deff_prop <- if (V_srs > 0) V_prop / V_srs else NA_real_
  deff_opt  <- if (V_srs > 0) V_opt  / V_srs else NA_real_

  # -- Equivalent SRS size to match Neyman precision ------------------------
  # V_opt = WSh^2/n  ->  n_SRS = S2/V_opt = S2*n/WSh^2
  n_srs_equiv <- ceiling(S2 * n_total / WSh^2)
  pct_saving  <- if (n_srs_equiv > 0)
    round(100 * (n_srs_equiv - n_total) / n_srs_equiv, 1) else NA_real_

  # -- Relative gains --------------------------------------------------------
  gain_over_prop <- if (V_prop > 0) V_prop / V_opt else NA_real_
  gain_over_srs  <- if (V_srs  > 0) V_srs  / V_opt else NA_real_

  structure(
    list(
      n              = n_total,
      H              = H,
      S2             = S2,
      V_within       = V_within,
      V_srs          = V_srs,
      V_prop         = V_prop,
      V_opt          = V_opt,
      SE_srs         = SE_srs,
      SE_prop        = SE_prop,
      SE_opt         = SE_opt,
      deff_prop      = deff_prop,
      deff_opt       = deff_opt,
      n_srs_equiv    = n_srs_equiv,
      pct_saving     = pct_saving,
      gain_over_prop = gain_over_prop,
      gain_over_srs  = gain_over_srs,
      WhShTot        = WSh
    ),
    class = "compare_designs"
  )
}

# =============================================================================
# Internal helper: estimate S^2 from a "strata" object
# =============================================================================
.S2_from_strata <- function(object) {

  # -- Raw data available -> exact population variance -------------------------
  pop <- object$data_internal
  if (!is.null(pop) && length(pop) >= 2L) {
    N  <- length(pop)
    mu <- mean(pop)
    return(sum((pop - mu)^2) / N)   # population variance (divide by N, not N-1)
  }

  # -- Distribution pathway -> numerical integration ---------------------------
  x_lo  <- object$maxval * object$initval
  x_hi  <- object$maxval * object$finval
  if (!is.finite(x_lo) || !is.finite(x_hi) || x_hi <= x_lo)
    return(sum(object$Wh * object$Vh))   # last-resort fallback

  x_seq <- seq(x_lo, x_hi, length.out = 1024L)

  params <- tryCatch({
    fit <- object$fit
    if (!is.null(fit$estimate))          fit$estimate
    else if (!is.null(fit$fit$estimate)) fit$fit$estimate
    else NULL
  }, error = function(e) NULL)

  if (is.null(params))
    return(sum(object$Wh * object$Vh))

  dens <- tryCatch(
    switch(object$distr,
      norm    = stats::dnorm(x_seq,    mean     = params["mean"],
                                        sd       = params["sd"]),
      lnorm   = stats::dlnorm(x_seq,   meanlog  = params["meanlog"],
                                        sdlog    = params["sdlog"]),
      gamma   = stats::dgamma(x_seq,   shape    = params["shape"],
                                        rate     = params["rate"]),
      weibull = stats::dweibull(x_seq, shape    = params["shape"],
                                        scale    = params["scale"]),
      exp     = stats::dexp(x_seq,     rate     = params["rate"]),
      cauchy  = stats::dcauchy(x_seq,  location = params["location"],
                                        scale    = params["scale"]),
      unif    = stats::dunif(x_seq,    min      = params["min"],
                                        max      = params["max"]),
      rep(1 / (x_hi - x_lo), 1024L)   # fallback: uniform over domain
    ),
    error = function(e) rep(1 / (x_hi - x_lo), 1024L)
  )
  dens[!is.finite(dens)] <- 0

  dx  <- diff(x_seq)
  trap <- function(f) sum(dx * (head(f, -1L) + tail(f, -1L))) / 2

  tot <- trap(dens)
  if (tot < 1e-12) return(sum(object$Wh * object$Vh))

  mu  <- trap(x_seq        * dens) / tot
  mu2 <- trap(x_seq ^ 2L  * dens) / tot

  max(mu2 - mu^2, 0)
}

# =============================================================================
# print method
# =============================================================================

#' @export
print.compare_designs <- function(x, digits = 6, ...) {

  cat("\n")
  cat("  -- Design Efficiency Comparison ------------------------------\n")
  cat(sprintf("     H = %d strata   |   n = %d   |   S^2 = %s\n\n",
              x$H, x$n, formatC(x$S2, format = "g", digits = digits)))

  cat("  Design                  V(ybar)              SE(ybar)    DEFF\n")
  cat("  -------------------------------------------------------------\n")
  cat(sprintf("  SRS  (baseline)    %s   %s   1.000\n",
              formatC(x$V_srs,  format = "e", digits = 4),
              formatC(x$SE_srs, format = "f", digits = digits)))
  cat(sprintf("  Proportional alloc %s   %s   %.4f\n",
              formatC(x$V_prop,  format = "e", digits = 4),
              formatC(x$SE_prop, format = "f", digits = digits),
              x$deff_prop))
  cat(sprintf("  Neyman (optimal)   %s   %s   %.4f\n",
              formatC(x$V_opt,  format = "e", digits = 4),
              formatC(x$SE_opt, format = "f", digits = digits),
              x$deff_opt))
  cat("  -------------------------------------------------------------\n")
  cat(sprintf("  Neyman vs SRS       :  %.2fx more efficient\n",
              x$gain_over_srs))
  cat(sprintf("  Neyman vs Propnl    :  %.2fx more efficient\n",
              x$gain_over_prop))
  cat(sprintf("  Equivalent SRS n    :  %d  (vs n = %d stratified)\n",
              x$n_srs_equiv, x$n))
  cat(sprintf("  Sample-size saving  :  %.1f%%\n", x$pct_saving))
  cat("\n")
  invisible(x)
}
