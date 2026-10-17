# =============================================================================
# strata.data.R  -  stratifyR 2.0
#
# Public function: strata.data()
#
# Changes from 1.x:
#   * Default method = "dp" (original Dynamic Programming solver, unchanged).
#   * method = "cobyla" provides a fast multi-start alternative.
#   * Returns the same "strata" S3 class as before (100 % backward-compatible).
#   * Adds extra slots to the "strata" object used by plot.strata() and
#     summary.strata().
# =============================================================================

#' Stratification of Univariate Survey Population Using the Data
#'
#' Computes Optimum Strata Boundaries (OSB) and Optimum Sample Sizes (OSS)
#' directly from a population dataset by (1) fitting the best-matching
#' parametric distribution via AIC, (2) formulating the stratification
#' problem as a Mathematical Programming Problem (MPP), and (3) solving it
#' with the original Dynamic Programming (DP) technique (default) or one of
#' three alternatives.
#'
#' The default solver (\code{method = "dp"}) uses the original Dynamic
#' Programming grid search, which is globally optimal on a 1,000-point grid
#' for the \emph{fitted} distribution. \code{method = "exact"} runs Dynamic
#' Programming on the data themselves and returns the partition with the
#' smallest empirical Neyman cost over all partitions with at least two units
#' per stratum. \code{method = "cobyla"} (multi-start COBYLA and BOBYQA) and
#' \code{method = "global"} (DIRECT-L followed by local refinement) are fast
#' continuous searches on the empirical objective.
#'
#' @param data    A numeric vector containing every unit of the survey
#'   population (the stratification variable \eqn{y}).
#' @param h       Integer. Number of strata (\eqn{h \geq 2}).
#' @param n       Integer. Fixed total sample size.
#' @param cost    Logical. \code{FALSE} (default) for equal stratum costs.
#'   Set \code{TRUE} and supply \code{ch} for cost-aware Neyman allocation.
#' @param ch      Numeric vector of length \code{h}. Per-stratum sampling
#'   costs; required when \code{cost = TRUE}.
#' @param method  Character. \code{"dp"} (default) is the original Dynamic
#'   Programming solver on the fitted distribution; \code{"exact"} is Dynamic
#'   Programming on the data (exact for the data; when there are more than
#'   20,000 distinct cut points it searches 20,000 cut points spread evenly in
#'   rank, see \code{options(stratifyR.exact_max_cand)}); \code{"cobyla"} and
#'   \code{"global"} are fast continuous solvers on the empirical objective.
#' @param n_starts Integer. Number of random restarts for COBYLA
#'   (ignored when \code{method = "dp"}).  Default \code{20L}.
#' @param max_iter Integer. Maximum function evaluations per COBYLA start.
#'   Default \code{2000L}.
#' @param tol     Numeric. Relative objective convergence tolerance for
#'   COBYLA.  Default \code{1e-9}.
#' @param verbose Logical. Print per-start progress when
#'   \code{method = "cobyla"}.  Default \code{FALSE}.
#'
#' @return An object of class \code{"strata"} (same structure as
#'   \strong{stratifyR} 1.x), with the following additional slots useful for
#'   \code{plot()}, \code{summary()}, and the OptiStrata ecosystem:
#'   \describe{
#'     \item{\code{optimality_gap}}{Cauchy-Schwarz fractional gap (COBYLA only).}
#'     \item{\code{kkt_residuals}}{First-order KKT residuals at the solution.}
#'     \item{\code{converged}}{Logical convergence flag.}
#'     \item{\code{method}}{Solver used.}
#'     \item{\code{exact_full}}{For \code{method = "exact"}: \code{TRUE} if every
#'       possible cut point was searched.}
#'     \item{\code{data_internal}}{The input data vector (used by \code{plot.strata}).}
#'   }
#'
#' @examples
#' \dontrun{
#' data <- rweibull(1000, shape = 2, scale = 1.5)
#' obj  <- strata.data(data, h = 4, n = 300)
#' summary(obj)
#' plot(obj)
#' plot(obj, type = "3d")
#' plot(obj, type = "interactive")
#'
#' # Use the fast COBYLA solver instead
#' obj_cobyla <- strata.data(data, h = 4, n = 300, method = "cobyla")
#' summary(obj_cobyla)
#'
#' # Real dataset
#' data(anaemia)
#' res <- strata.data(anaemia$Iron, h = 3, n = 350)
#' summary(res)
#' plot(res)
#' }
#'
#' @seealso \code{\link{strata.distr}}, \code{\link{plot.strata}},
#'   \code{\link{summary.strata}}
#'
#' @references
#' Khan, M.G.M., Khan, E.A., Ahsan, M.J. (2002). An optimal multivariate
#' stratified sampling design using dynamic programming.
#' \emph{Applied Mathematics Letters}, \strong{15}(7), 838-844.
#'
#' Reddy, K.G. & Khan, M.G.M. (2018). Determining the Optimum Stratum
#' Boundaries: An Analytical Approach.
#' \emph{Australian and New Zealand Journal of Statistics}, \strong{60}(4),
#' 437-454. \doi{10.1111/anzs.12244}
#'
#' @export
strata.data <- function(data,
                         h,
                         n,
                         cost     = FALSE,
                         ch       = NULL,
                         method   = c("dp", "cobyla", "global", "exact"),
                         n_starts = 20L,
                         max_iter = 2000L,
                         tol      = 1e-9,
                         verbose  = FALSE) {

  method <- match.arg(method)

  # ---- input validation ------------------------------------------------------
  if (missing(data)) stop("'data' must be specified")
  if (missing(h))    stop("'h' must be specified")
  if (missing(n))    stop("'n' must be specified")
  if (n > length(data))
    stop("'n' cannot be greater than N (length of data)")

  data_raw <- as.numeric(data)

  # ---- build shared environment (same layout as v1.x) -----------------------
  my_env <- new.env(parent = emptyenv())
  my_env$h <- h
  my_env$n <- n
  N <- length(data_raw)
  my_env$N <- N

  my_env$cost <- cost
  if (isTRUE(cost)) {
    if (length(ch) != h)
      stop("'ch' must have length equal to h")
    my_env$ch <- ch
  } else {
    my_env$ch <- rep(1, h)
  }

  # Replace zeros
  if (any(data_raw == 0)) data_raw[data_raw == 0] <- 1e-5

  my_env$maxval  <- max(data_raw)
  scaled_data    <- data_raw / my_env$maxval
  my_env$initval <- min(scaled_data)
  my_env$finval  <- max(scaled_data)
  my_env$dist    <- my_env$finval - my_env$initval

  # Fit best-matching distribution to scaled data
  my_env$obj <- get.dist(scaled_data, my_env)

  # DP constants (needed even when method="cobyla" for data.alloc compatibility)
  my_env$z       <- 100
  my_env$factor  <- 4
  my_env$inc     <- 0.001
  my_env$inc2    <- 0.00001
  my_env$points  <- 1000
  my_env$stages  <- h + 1
  my_env$ylimits <- integer(h + 1)
  my_env$p       <- max(1L, as.integer(my_env$dist * my_env$points))
  my_env$e       <- max(1L, as.integer(my_env$dist * my_env$points * my_env$z) + 1L)

  # Store real-scale sorted data for empirical objective (cobyla + global methods)
  my_env$data_sorted_real <- sort(data_raw)

  # ---- solve -----------------------------------------------------------------
  if (method == "cobyla") {
    OSB <- .solve_cobyla_data(data_raw, h, n, N, my_env,
                               n_starts, max_iter, tol, verbose)
  } else if (method == "global") {
    OSB <- .solve_global_data(h, n, N, my_env, max_iter, tol, verbose)
  } else if (method == "exact") {
    OSB <- .solve_exact_data(data_raw, h, n, N, my_env)
  } else {
    OSB <- .solve_dp_data(h, n, N, my_env)
  }

  # ---- extract outputs -------------------------------------------------------
  converged      <- attr(OSB, "converged")
  optimality_gap <- attr(OSB, "optimality_gap")
  kkt_resid      <- attr(OSB, "kkt_residuals")

  h_df   <- data.frame("Strata" = 1:h)
  distr  <- as.character(my_env$obj["distr"])

  # Handle triangle vs rtriangle
  # (right-triangular means the mode is at the minimum; v2.0-2 compared the
  # mode with the maximum by mistake)
  trng_min  <- tryCatch(my_env$obj[["params"]]["min"],  error = function(e) NA)
  trng_mode <- tryCatch(my_env$obj[["params"]]["mode"], error = function(e) NA)
  if (identical(distr, "triangle") &&
      !is.na(trng_min) && !is.na(trng_mode) &&
      round(trng_min, 1) == round(trng_mode, 1)) {
    distr <- "rtriangle"
  }

  # Re-fit distribution on REAL-scale data for the fit slot
  fit <- .refit_real_scale(data_raw, distr, my_env)

  # Compute stratum Wh, Sh, WhSh from raw data using OSB
  data.alloc(data_raw, my_env)
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

  # Extra slots (v2.0 additions - backward-compatible; ignored by old code)
  out4 <- list(optimality_gap  = optimality_gap,
               kkt_residuals   = kkt_resid,
               converged       = converged,
               method          = method,
               exact_full      = attr(OSB, "exact_full"),
               data_internal   = data_raw)   # for plot.strata()

  result <- c(out1, out2, out3, out4)
  class(result) <- "strata"
  result
}


# =============================================================================
# COBYLA solution path for strata.data
# =============================================================================
.solve_cobyla_data <- function(data_raw, h, n, N, my_env,
                                n_starts, max_iter, tol, verbose) {

  sorted_data <- sort(data_raw / my_env$maxval)
  # data_sorted_real already set in strata.data() before dispatch

  res <- .cobyla_run(
    my_env      = my_env,
    H           = h,
    use_distr   = FALSE,
    n_starts    = n_starts,
    max_iter    = max_iter,
    tol         = tol,
    verbose     = verbose,
    data_sorted = sorted_data
  )

  # Degeneracy check: if COBYLA still collapsed a stratum, fall back to DP.
  # This guards against cases where the constraint tolerance still allows
  # near-duplicate boundaries (which produce empty strata and wrong results).
  if (.cobyla_is_degenerate(res$b_dist, my_env, h)) {
    if (verbose)
      message("[COBYLA] Degenerate solution detected (collapsed stratum); ",
              "falling back to DP solver for correctness.")
    return(.solve_dp_data(h, n, N, my_env))
  }

  # Boundaries in real scale
  res$b_dist <- .snap_data_bdist(res$b_dist, my_env)
  OSB <- .cobyla_real_osb(res$b_dist, my_env)

  # Store OSB in my_env for data.alloc
  .store_osb_env(OSB, my_env, h)

  # Compute optimality gap & KKT residuals
  ss     <- .cobyla_stratum_stats(res$b_dist, my_env, h, use_distr = FALSE)
  gap_ls <- .cobyla_gap(ss$WhSh, ss$Sh, ss$Wh, n, N)
  kkt    <- .cobyla_kkt(res$b_dist, ss$Wh, ss$Sh, .approx_mu(res$b_dist, my_env), my_env)

  attr(OSB, "converged")       <- res$converged
  attr(OSB, "optimality_gap")  <- gap_ls$gap
  attr(OSB, "kkt_residuals")   <- kkt
  OSB
}


# =============================================================================
# Global solution path for strata.data (DIRECT-L + COBYLA refinement)
# =============================================================================
.solve_global_data <- function(h, n, N, my_env, max_eval_local, tol, verbose) {

  # data_sorted_real is set centrally in strata.data() before dispatch

  res <- .global_run(
    my_env         = my_env,
    H              = h,
    use_distr      = FALSE,
    max_eval_local = max_eval_local,
    tol            = tol,
    verbose        = verbose
  )

  # Safety net: if still degenerate, fall back to DP
  if (.cobyla_is_degenerate(res$b_dist, my_env, h)) {
    if (verbose)
      message("[GLOBAL] Degenerate solution; falling back to DP.")
    return(.solve_dp_data(h, n, N, my_env))
  }

  res$b_dist <- .snap_data_bdist(res$b_dist, my_env)
  OSB <- .cobyla_real_osb(res$b_dist, my_env)
  .store_osb_env(OSB, my_env, h)

  ss     <- .cobyla_stratum_stats(res$b_dist, my_env, h, use_distr = FALSE)
  gap_ls <- .cobyla_gap(ss$WhSh, ss$Sh, ss$Wh, n, N)
  kkt    <- .cobyla_kkt(res$b_dist, ss$Wh, ss$Sh,
                         .approx_mu(res$b_dist, my_env), my_env)

  attr(OSB, "converged")      <- res$converged
  attr(OSB, "optimality_gap") <- gap_ls$gap
  attr(OSB, "kkt_residuals")  <- kkt
  OSB
}


# =============================================================================
# DP solution path for strata.data (unchanged internals from v1.x)
# =============================================================================
.solve_dp_data <- function(h, n, N, my_env) {
  create.mat(my_env)
  z      <- my_env$z;  factor <- my_env$factor
  inc    <- my_env$inc;  inc2 <- my_env$inc2
  points <- my_env$points;  p <- my_env$p;  e <- my_env$e

  my_env$ObjFV <- data.optim(k = h, n = p, incf = inc,
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

  my_env$ObjFV <- data.optim(k = h, n = e - 1L, incf = inc2,
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
  polished <- .dp_polish(my_env, h, use_distr = FALSE)
  # v2.0-2: keep full precision and make the top boundary the data maximum
  # (rounding the top boundary down used to drop the largest unit(s)).
  OSB <- my_env$maxval * my_env$df$x
  OSB[h] <- my_env$maxval * my_env$finval
  # On very heavy-tailed data the fitted model can make the DP recursion fail
  # (no feasible split for some stratum). Say so plainly instead of failing
  # later with an obscure error.
  if (any(!is.finite(OSB)) || is.unsorted(OSB, strictly = TRUE))
    stop("The DP solver could not find valid boundaries for the fitted '",
         as.character(my_env$obj["distr"]), "' distribution (the fit is probably poor in ",
         "the tail). Use method = \"exact\" or method = \"cobyla\" for these data.",
         call. = FALSE)
  .store_osb_env(OSB, my_env, h)

  attr(OSB, "converged")      <- TRUE
  attr(OSB, "optimality_gap") <- NA_real_
  attr(OSB, "kkt_residuals")  <- rep(NA_real_, h - 1L)
  OSB
}


# =============================================================================
# Helper: store OSB into my_env so data.alloc can read them
# =============================================================================
.store_osb_env <- function(OSB, my_env, h) {
  my_env$df <- data.frame(
    h = seq_len(h),
    d = NA_real_,
    y = NA_real_,
    x = c(as.numeric(OSB)[seq_len(h - 1L)] / my_env$maxval, my_env$finval)
  )
}


# =============================================================================
# Helper: approximate per-stratum means for KKT check (midpoint approx)
# =============================================================================
.approx_mu <- function(b_dist, my_env) {
  H      <- length(b_dist) + 1L
  b_full <- c(0, sort(b_dist), my_env$dist)
  mu_h   <- numeric(H)
  for (h in seq_len(H)) {
    lo <- my_env$initval + b_full[h]
    hi <- my_env$initval + b_full[h + 1L]
    mu_h[h] <- (lo + hi) / 2
  }
  mu_h
}


# =============================================================================
# Helper: re-fit distribution to REAL-scale data for the fit slot
# =============================================================================
.refit_real_scale <- function(data_raw, distr, my_env) {
  # v2.0-3: the Pareto and triangular families are not refitted with
  # fitdistrplus (which needs dpareto()/dtriang() on the user's search path);
  # their scaled-data estimates are converted to the real scale instead. The
  # same conversion is the fallback if any refit fails, so the fit slot is
  # always on the scale of the data.
  real <- list(distr = distr,
               estimate = .rescale_params(distr, my_env$obj[["params"]], my_env$maxval))
  if (distr %in% c("pareto", "triangle", "rtriangle")) return(real)
  tryCatch(
    fitdistrplus::fitdist(data_raw, distr = distr, method = "mle"),
    error = function(e) real)
}

# Convert distribution parameters fitted on data / maxval back to the scale of
# the data.
.rescale_params <- function(distr, p, maxval) {
  p <- unlist(p)
  sc <- function(nm) if (nm %in% names(p)) p[[nm]] <<- p[[nm]] * maxval
  switch(distr,
    pareto  = sc("scale"),
    weibull = sc("scale"),
    gamma   = if ("rate" %in% names(p)) p[["rate"]] <- p[["rate"]] / maxval,
    exp     = if ("rate" %in% names(p)) p[["rate"]] <- p[["rate"]] / maxval,
    lnorm   = if ("meanlog" %in% names(p)) p[["meanlog"]] <- p[["meanlog"]] + log(maxval),
    norm    = { sc("mean"); sc("sd") },
    cauchy  = { sc("location"); sc("scale") },
    unif    = { sc("min"); sc("max") },
    triangle = , rtriangle = { sc("min"); sc("max"); sc("mode") },
    NULL)
  p
}
