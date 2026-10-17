# =============================================================================
# strata.distr.R  -  stratifyR 2.0
#
# Public function: strata.distr()
#
# Changes from 1.x:
#   * Default method = "dp" (original Dynamic Programming solver, unchanged).
#   * method = "cobyla" provides a fast multi-start alternative.
#   * Returns the same "strata" S3 class as before (100% backward-compatible).
#   * Adds extra diagnostic slots (optimality_gap, kkt_residuals, converged).
# =============================================================================

#' Stratification of Univariate Survey Population Using the Distribution
#'
#' Computes Optimum Strata Boundaries (OSB) and Optimum Sample Sizes (OSS)
#' when the dataset of the stratification variable is not available, relying
#' instead on a hypothesised parametric distribution and its parameter values.
#'
#' The default solver (\code{method = "dp"}) uses the original Dynamic
#' Programming grid search, which guarantees a globally optimal solution.
#' The alternative COBYLA solver (\code{method = "cobyla"}) uses a fast
#' multi-start nonlinear optimiser; its solution quality is validated by the
#' Cauchy-Schwarz lower-bound gap stored in \code{result$optimality_gap}.
#'
#' @param h       Integer. Number of strata.
#' @param initval Numeric. Initial (minimum) value of the population variable.
#' @param dist    Numeric. Range (distance) of the population variable.
#' @param distr   Character. Name of the assumed distribution; one of
#'   \code{"pareto"}, \code{"triangle"}, \code{"rtriangle"}, \code{"weibull"},
#'   \code{"gamma"}, \code{"exp"}, \code{"unif"}, \code{"norm"},
#'   \code{"lnorm"}, \code{"cauchy"}.
#' @param params  Named numeric vector of distribution parameters.
#' @param n       Integer. Fixed total sample size.
#' @param N       Integer. Fixed total population size.
#' @param cost    Logical. Default \code{FALSE} (equal unit costs).
#' @param ch      Numeric vector of stratum costs (required when
#'   \code{cost = TRUE}).
#' @param method  Character. \code{"dp"} (default, Dynamic Programming solver)
#'   or \code{"cobyla"} (fast multi-start alternative).
#' @param n_starts Integer. Number of COBYLA random restarts.  Default
#'   \code{20L}.
#' @param max_iter Integer. Max evaluations per COBYLA start.  Default
#'   \code{2000L}.
#' @param tol     Numeric. COBYLA convergence tolerance.  Default \code{1e-9}.
#' @param verbose Logical. Print per-start diagnostics.  Default \code{FALSE}.
#'
#' @return An object of class \code{"strata"} (same layout as
#'   \strong{stratifyR} 1.x), with additional v2.0 diagnostic slots.
#'
#' @examples
#' \dontrun{
#' res <- strata.distr(h = 2, initval = 1.5, dist = 33,
#'                     distr = "weibull",
#'                     params = c(shape = 2.15, scale = 13.5),
#'                     n = 500, N = 2000)
#' summary(res)
#' plot(res)
#' plot(res, type = "3d")
#' plot(res, type = "interactive")
#'
#' # Log-normal example
#' res2 <- strata.distr(h = 3, initval = 1, dist = 10415,
#'                      distr  = "lnorm",
#'                      params = c(meanlog = 5.5, sdlog = 1.5),
#'                      n = 500, N = 12000)
#' summary(res2)
#' }
#'
#' @seealso \code{\link{strata.data}}, \code{\link{plot.strata}},
#'   \code{\link{summary.strata}}
#'
#' @references
#' Khan, M.G.M. et al. (2008). Determining the optimum strata boundaries
#' using mathematical programming. \emph{Survey Methodology}, \strong{34}(2),
#' 91-102.
#'
#' @export
strata.distr <- function(h,
                          initval,
                          dist,
                          distr   = c("pareto", "triangle", "rtriangle",
                                      "weibull", "gamma", "exp", "unif",
                                      "norm", "lnorm", "cauchy"),
                          params  = c(shape = 0, scale = 0, rate = 0,
                                      gamma = 0, location = 0, mean = 0,
                                      sd = 0, meanlog = 0, sdlog = 0,
                                      min = 0, max = 0, mode = 0),
                          n,
                          N,
                          cost     = FALSE,
                          ch       = NULL,
                          method   = c("dp", "cobyla", "global"),
                          n_starts = 20L,
                          max_iter = 2000L,
                          tol      = 1e-9,
                          verbose  = FALSE) {

  method <- match.arg(method)

  # ---- validation ------------------------------------------------------------
  if (missing(h))       stop("Number of strata ('h') must be specified")
  if (missing(initval)) stop("Initial value ('initval') must be specified")
  if (missing(dist))    stop("Range ('dist') must be specified")
  if (missing(distr))   stop("Distribution ('distr') must be specified")
  if (missing(params))  stop("Parameters ('params') must be specified")
  if (missing(n) && missing(N))
    stop("'n' or 'N' must be specified")

  # v2.0-3: a bounded distribution must have some probability in the range
  # [initval, initval + dist]; otherwise every stratum would be empty.
  if (distr[1] %in% c("unif", "triangle", "rtriangle") &&
      all(c("min", "max") %in% names(params))) {
    lo_s <- params[["min"]]; hi_s <- params[["max"]]
    if (initval + dist <= lo_s || initval >= hi_s)
      stop(sprintf(paste0("The range [initval, initval + dist] = [%g, %g] lies outside ",
                          "the support [%g, %g] of the '%s' distribution."),
                   initval, initval + dist, lo_s, hi_s, distr[1]), call. = FALSE)
  }

  # ---- build shared environment ----------------------------------------------
  my_env <- new.env(parent = emptyenv())
  my_env$h <- h
  my_env$n <- n
  my_env$N <- N
  my_env$cost <- cost

  if (isTRUE(cost)) {
    if (length(ch) != h)
      stop("'ch' vector must have length equal to h")
    my_env$ch <- ch
  } else {
    my_env$ch <- rep(1, h)
  }

  # Scale parameters (same as v1.x)
  my_env$maxval  <- initval + dist
  my_env$initval <- initval / my_env$maxval
  my_env$finval  <- 1.0
  my_env$dist    <- my_env$finval - my_env$initval

  my_env$obj <- list("distr" = distr, "params" = params)

  # DP constants
  my_env$z       <- 100
  my_env$factor  <- 4
  my_env$inc     <- 0.001
  my_env$inc2    <- 0.00001
  my_env$points  <- 1000
  my_env$stages  <- h + 1
  my_env$ylimits <- integer(h + 1)
  my_env$p       <- max(1L, as.integer(my_env$dist * my_env$points))
  my_env$e       <- max(1L, as.integer(my_env$dist * my_env$points * my_env$z) + 1L)

  # ---- solve -----------------------------------------------------------------
  if (method == "cobyla") {
    OSB <- .solve_cobyla_distr(h, n, N, my_env, n_starts, max_iter, tol, verbose)
  } else if (method == "global") {
    OSB <- .solve_global_distr(h, n, N, my_env, max_iter, tol, verbose)
  } else {
    OSB <- .solve_dp_distr(h, n, N, my_env)
  }

  # ---- extract attributes ----------------------------------------------------
  converged      <- attr(OSB, "converged")
  optimality_gap <- attr(OSB, "optimality_gap")
  kkt_resid      <- attr(OSB, "kkt_residuals")

  h_df   <- data.frame("Strata" = 1:h)
  distr  <- as.character(my_env$obj["distr"])
  params <- my_env$obj[["params"]]
  fit    <- list("distr" = distr, "estimate" = params)

  # Compute stratum allocation
  distr.alloc(my_env)
  Output  <- my_env$output
  out_smp <- my_env$out
  deno    <- my_env$deno
  WhTot   <- my_env$WhTot
  NhTot   <- my_env$NhTot
  nhTot   <- my_env$nhTot
  fhTot   <- my_env$fhTot
  VhTot   <- my_env$VhTot

  # ---- assemble strata object ------------------------------------------------
  out1 <- list(cost = cost, distr = distr, fit = fit, n = n, N = N, ch = ch,
               maxval  = my_env$maxval,
               initval = my_env$initval,
               finval  = my_env$finval,
               dist    = my_env$dist)

  out2 <- list(h = h_df, OSB = OSB,
               Wh   = Output$Wh,
               Vh   = Output$Vh,
               WhSh = Output$WhSh,
               nh   = out_smp$nh,
               Nh   = out_smp$Nh,
               fh   = out_smp$fh)

  out3 <- list(WhTot   = WhTot,
               VhTot   = VhTot,
               WhShTot = deno,
               nhTot   = nhTot,
               NhTot   = NhTot,
               fhTot   = fhTot)

  out4 <- list(optimality_gap  = optimality_gap,
               kkt_residuals   = kkt_resid,
               converged       = converged,
               method          = method,
               data_internal   = NULL)   # no raw data for distr pathway

  result <- c(out1, out2, out3, out4)
  class(result) <- "strata"
  result
}


# =============================================================================
# COBYLA solution path for strata.distr
# =============================================================================
.solve_cobyla_distr <- function(h, n, N, my_env,
                                 n_starts, max_iter, tol, verbose) {
  res <- .cobyla_run(
    my_env      = my_env,
    H           = h,
    use_distr   = TRUE,
    n_starts    = n_starts,
    max_iter    = max_iter,
    tol         = tol,
    verbose     = verbose,
    data_sorted = NULL
  )

  # Degeneracy check: fall back to DP if any stratum collapsed to near-zero width.
  if (.cobyla_is_degenerate(res$b_dist, my_env, h)) {
    if (verbose)
      message("[COBYLA] Degenerate solution detected (collapsed stratum); ",
              "falling back to DP solver for correctness.")
    return(.solve_dp_distr(h, n, N, my_env))
  }

  OSB <- .cobyla_real_osb(res$b_dist, my_env)
  .store_osb_env(OSB, my_env, h)

  ss     <- .cobyla_stratum_stats(res$b_dist, my_env, h, use_distr = TRUE)
  gap_ls <- .cobyla_gap(ss$WhSh, ss$Sh, ss$Wh, n, N)
  kkt    <- .cobyla_kkt(res$b_dist, ss$Wh, ss$Sh,
                         .approx_mu(res$b_dist, my_env), my_env)

  attr(OSB, "converged")      <- res$converged
  attr(OSB, "optimality_gap") <- gap_ls$gap
  attr(OSB, "kkt_residuals")  <- kkt
  OSB
}


# =============================================================================
# Global solution path for strata.distr (DIRECT-L + COBYLA refinement)
# =============================================================================
.solve_global_distr <- function(h, n, N, my_env, max_eval_local, tol, verbose) {

  res <- .global_run(
    my_env         = my_env,
    H              = h,
    use_distr      = TRUE,
    max_eval_local = max_eval_local,
    tol            = tol,
    verbose        = verbose
  )

  # Safety net: if still degenerate, fall back to DP
  if (.cobyla_is_degenerate(res$b_dist, my_env, h)) {
    if (verbose)
      message("[GLOBAL] Degenerate solution; falling back to DP.")
    return(.solve_dp_distr(h, n, N, my_env))
  }

  OSB <- .cobyla_real_osb(res$b_dist, my_env)
  .store_osb_env(OSB, my_env, h)

  ss     <- .cobyla_stratum_stats(res$b_dist, my_env, h, use_distr = TRUE)
  gap_ls <- .cobyla_gap(ss$WhSh, ss$Sh, ss$Wh, n, N)
  kkt    <- .cobyla_kkt(res$b_dist, ss$Wh, ss$Sh,
                         .approx_mu(res$b_dist, my_env), my_env)

  attr(OSB, "converged")      <- res$converged
  attr(OSB, "optimality_gap") <- gap_ls$gap
  attr(OSB, "kkt_residuals")  <- kkt
  OSB
}


# =============================================================================
# DP solution path for strata.distr (unchanged from v1.x)
# =============================================================================
.solve_dp_distr <- function(h, n, N, my_env) {
  create.mat(my_env)
  z      <- my_env$z;  factor <- my_env$factor
  inc    <- my_env$inc;  inc2 <- my_env$inc2
  points <- my_env$points;  p <- my_env$p;  e <- my_env$e

  my_env$ObjFV <- distr.optim(k = h, n = p, incf = inc,
                               minYk = 0, maxYk = p,
                               isFirstRun = TRUE, my_env)

  d <- double(h); y <- double(h); x <- double(h); temp <- 0

  for (i in h:1) {
    if (i == h) {
      d[i] <- my_env$dist
      y[i] <- my_env$dk2[i + 1L, p + 1L]
      x[i] <- my_env$initval + my_env$dist
    } else if (i == 1L) {
      d[i] <- d[i + 1L] - y[i + 1L]
      y[i] <- d[i]
      x[i] <- y[i] + my_env$initval
    } else {
      d[i] <- d[i + 1L] - y[i + 1L]
      temp <- min(max(as.integer(round(d[i] * points)), 0L), ncol(my_env$dk2) - 1L)   # v2.0-3: round, not truncate
      y[i] <- my_env$dk2[i + 1L, temp + 1L]
      x[i] <- x[i + 1L] - y[i + 1L]
    }
  }

  d1 <- d; y1 <- y; x1 <- x   # first (coarse) pass, kept as a fallback
  for (i in h:1) my_env$ylimits[i + 1L] <- as.integer(round(y[i] * points * z))

  my_env$ObjFV <- distr.optim(k = h, n = e - 1L, incf = inc2,
                               minYk = my_env$ylimits[h + 1L] - my_env$factor * my_env$z,
                               maxYk = my_env$ylimits[h + 1L] + my_env$factor * my_env$z,
                               isFirstRun = FALSE, my_env)

  for (i in h:1) {
    if (i == h) {
      d[i] <- my_env$dist
      y[i] <- my_env$dk2[i + 1L, e]
      x[i] <- my_env$initval + my_env$dist
    } else if (i == 1L) {
      d[i] <- d[i + 1L] - y[i + 1L]
      y[i] <- d[i]
      x[i] <- y[i] + my_env$initval
    } else {
      d[i] <- d[i + 1L] - y[i + 1L]
      temp <- min(max(as.integer(round(d[i] * points * z)), 0L), ncol(my_env$dk2) - 1L)   # v2.0-3: round, not truncate
      y[i] <- my_env$dk2[i + 1L, temp + 1L]
      x[i] <- x[i + 1L] - y[i + 1L]
    }
  }

  # v2.0-3: the fine pass only fills a window of the table around the coarse
  # answer. If its answer moves to the edge of that window, the earlier
  # stages can look up cells that were never filled (NA). Then keep the
  # coarse answer; the continuous polish below refines it anyway.
  if (any(!is.finite(c(d, y, x))) || is.unsorted(x, strictly = TRUE)) {
    d <- d1; y <- y1; x <- x1
  }
  my_env$df <- data.frame(h, d, y, x)
  # v2.0-3: remove the grid error by a continuous polish on the same model
  polished <- .dp_polish(my_env, h, use_distr = TRUE)
  # v2.0-2: keep full precision and make the top boundary the range end
  OSB <- my_env$maxval * my_env$df$x
  OSB[h] <- my_env$maxval * my_env$finval
  .store_osb_env(OSB, my_env, h)

  attr(OSB, "converged")      <- TRUE
  attr(OSB, "optimality_gap") <- NA_real_
  attr(OSB, "kkt_residuals")  <- rep(NA_real_, h - 1L)
  OSB
}
