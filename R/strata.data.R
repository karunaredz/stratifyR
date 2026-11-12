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
#' This function takes in the univariate population data (argument \code{data})
#' and a fixed sample size (n) to compute the optimum stratum boundaries (OSB)
#' for a given number of strata (L), optimum sample sizes (nh), etc. directly
#' from the data. The main idea used is from Khan et al (2008) whereby the
#' problem of stratification is formulated into a Mathematical Programming
#' Problem (MPP) using the best-fit frequency distribution and its parameters
#' estimated from the data. This MPP is then solved for the OSB using a Dynamic
#' Programming (DP) solution procedure.
#'
#'
#' @param data A vector of values of the survey variable y for which the OSB
#' are determined.
#' @param h A numeric: denotes the number of strata to be created.
#' @param n A numeric: denotes a fixed total sample size.
#' @param cost A logical: has default cost=FALSE. If it is a stratum-cost
#' problem, cost=TRUE, with which, one must provide the Ch parameter.
#' @param ch A numeric: denotes a vector of stratum costs. When cost=FALSE, it
#' has a default of NULL.
#' @param method Character. Optimisation method. \code{"dp"} (default) is the
#' original Dynamic Programming solver, globally optimal on a 1,000-point grid
#' for the fitted distribution. \code{"exact"} runs Dynamic Programming on the
#' data and returns the partition with the smallest empirical Neyman cost over
#' all partitions with at least two units per stratum; with more than 20,000
#' distinct cut points it searches 20,000 cut points spread evenly in rank (set
#' \code{options(stratifyR.exact_max_cand = )} to change this). \code{"cobyla"}
#' is a fast multi-start search (COBYLA and BOBYQA) on the empirical objective,
#' and \code{"global"} is a DIRECT-L global search followed by local
#' refinement.
#' @param n_starts Integer. Number of random restarts for the COBYLA solver.
#' Default \code{20L}. Used only by \code{"cobyla"}.
#' @param max_iter Integer. Maximum function evaluations per COBYLA start.
#' Default \code{2000L}.
#' @param tol Numeric. Convergence tolerance for COBYLA. Default \code{1e-9}.
#' @param verbose Logical. Print per-start progress. Default \code{FALSE}.
#' @param fpc Logical. With \code{method = "exact"} only. If \code{TRUE}, the
#' solver minimises the full variance of the stratified mean under Neyman
#' allocation, including the finite population correction, \eqn{V = (\sum W_h
#' S_h)^2/n - \sum W_h S_h^2/N}, instead of \eqn{\sum W_h S_h}. The minimum is
#' certified when the reported \code{optimality_gap} is 0. Default
#' \code{FALSE}.
#' @param takeall Logical. With \code{method = "exact"} only. If \code{TRUE},
#' the top stratum may be made a take-all stratum, sampled completely, when
#' this gives a lower variance; the rest of the sample is allocated by Neyman
#' allocation over the other strata. Implies \code{fpc = TRUE}. Default
#' \code{FALSE}.
#' @return \code{strata.data} returns an object of class \code{"strata"} with
#' the Optimum Strata Boundaries (OSB; the upper boundary of each stratum, the
#' last being the largest data value), stratum weights (Wh), stratum variances
#' (Vh), Optimum Sample Sizes (nh), stratum population sizes (Nh) and sampling
#' fraction (fh). It also contains \code{variance}, the variance of the
#' stratified mean with the finite population correction for the final
#' allocation, \code{takeall}, the number of units in a take-all top stratum (0
#' if there is none), and \code{fpc}, which records whether the full variance
#' was minimised.
#' @author Karuna Reddy <karuna.reddy@@auckland.ac.nz>\cr MGM Khan
#' <mgm.khan@@usp.ac.fj>
#' @seealso \code{strata.distr}
#' @examples
#' 
#' \donttest{
#' data(sugarcane)
#' dp    <- strata.data(sugarcane$Production, h = 4, n = 500)
#' exact <- strata.data(sugarcane$Production, h = 4, n = 500, method = "exact")
#' c(dp = dp$WhShTot, exact = exact$WhShTot)
#' 
#' # minimise the full variance, allowing a take-all top stratum
#' set.seed(1)
#' x  <- (runif(2000)^(-1/2) - 1)          # heavy-tailed (Pareto II) population
#' ta <- strata.data(x, h = 4, n = 200, method = "exact", takeall = TRUE)
#' c(variance = ta$variance, takeall = ta$takeall)
#' }
#' \dontrun{
#' data <- rweibull(1000, shape=2, scale = 1.5)
#' hist(data)
#' obj <- strata.data(data, h = 2, n=300)
#' summary(obj)
#' #-------------------------------------------------------------
#' data(anaemia)
#' Iron <- anaemia$Iron
#' res <- strata.data(Iron, h = 2, n=350)
#' summary(res)
#' #-------------------------------------------------------------
#' data(SHS) #Household Spending data from stratification package
#' weight <- SHS$WEIGHT
#' hist(weight); length(weight)
#' res <- strata.data(weight, h = 2, n=500)
#' summary(res)
#' #-------------------------------------------------------------
#' data(sugarcane)
#' Production <- sugarcane$Production
#' hist(Production)
#' res <- strata.data(Production, h = 2, n=1000)
#' summary(res)
#' #-------------------------------------------------------------
#' #The function be dynamically used to visualize the the strata boundaries,
#' #for 2 strata, over the density (or observations) of the "mag" variable
#' #from the quakes data (with purrr and ggplot2 packages loaded).
#' output <- quakes %>%
#'           pluck("mag") %>%
#'           strata.data(h = 2, n = 300)
#' quakes %>%
#'       ggplot(aes(x = mag)) +
#'       geom_density(fill = "blue", colour = "black", alpha = 0.3) +
#'       geom_vline(xintercept = output$OSB, linetype = "dotted", color = "red")
#' #-------------------------------------------------------------
#' }
#' 
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
                         verbose  = FALSE,
                         fpc      = FALSE,
                         takeall  = FALSE) {

  method <- .check_method(method, c("dp", "cobyla", "global", "exact"))

  # ---- input checks (v2.1-0) -------------------------------------------------
  .check_h(h)
  .check_data(data, h)
  .check_n(n, h, N = length(data))
  .check_cost(cost, ch, h)
  .check_flag(fpc, "fpc")
  .check_flag(takeall, "takeall")
  if (method %in% c("cobyla", "global"))
    .check_solver_settings(n_starts, max_iter, tol, verbose)

  full_var <- isTRUE(fpc) || isTRUE(takeall)
  if (full_var && method != "exact")
    stop("'fpc' and 'takeall' are available with method = \"exact\" only.",
         call. = FALSE)
  if (full_var && isTRUE(cost))
    stop("'fpc' and 'takeall' cannot be combined with cost = TRUE.", call. = FALSE)

  data_raw <- as.numeric(data)

  # ---- build shared environment (same layout as v1.x) -----------------------
  my_env <- new.env(parent = emptyenv())
  my_env$h <- h
  my_env$n <- n
  N <- length(data_raw)
  my_env$N <- N

  my_env$cost <- cost
  if (isTRUE(cost)) {
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
  my_env$takeall_size <- 0L

  # ---- solve -----------------------------------------------------------------
  if (method == "cobyla") {
    OSB <- .solve_cobyla_data(data_raw, h, n, N, my_env,
                               n_starts, max_iter, tol, verbose)
  } else if (method == "global") {
    OSB <- .solve_global_data(h, n, N, my_env, max_iter, tol, verbose)
  } else if (method == "exact") {
    OSB <- if (full_var)
      .solve_exact_fpc(data_raw, h, n, N, my_env, takeall = isTRUE(takeall))
    else
      .solve_exact_data(data_raw, h, n, N, my_env)
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
               variance        = my_env$variance,
               takeall         = my_env$takeall_size,
               fpc             = full_var,
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
  # The optimiser tries impossible trial values (for example a negative
  # Weibull shape) on the way to the estimate; the warnings these give are
  # harmless, so they are muffled here as in get.dist().
  tryCatch(
    suppressWarnings(fitdistrplus::fitdist(data_raw, distr = distr, method = "mle")),
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
