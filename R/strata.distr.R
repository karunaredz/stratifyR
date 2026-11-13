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

#' Stratify a univariate population using a distribution
#'
#' This function takes in the underlying hypothetical distribution and its
#' parameter(s) of the survey variable, the initial value and the range of the
#' population, the fixed sample size (n) and the fixed population size (N) to
#' compute the optimum stratum boundaries (OSB) for a given number of strata
#' (L), optimum sample sizes (nh), etc. The main idea used is from Khan et al.
#' (2008) whereby the problem of stratification is fromulated into a
#' Mathematical Programming Problem (MPP) using the best-fit frequency
#' distribution and its parameter estimates of the data. This MPP is then
#' solved for the optimal solutions using the Dynamic Programming (DP) solution
#' procedure.
#'
#'
#' @param h The number of strata, a whole number of at least 2.
#' @param initval The smallest value of the stratification variable.
#' @param dist The range of the variable (largest minus smallest value), a
#' positive number.
#' @param distr A character: denotes the name of the distribution that
#' characterizes the population.
#' @param params A named numeric vector (or list) with the parameters of the
#' distribution: \code{shape} and \code{scale} for \code{"pareto"} and
#' \code{"weibull"}; \code{shape} and \code{rate} for \code{"gamma"};
#' \code{rate} for \code{"exp"}; \code{min} and \code{max} for
#' \code{"unif"} and \code{"rtriangle"}, with \code{mode} also for
#' \code{"triangle"}; \code{mean} and \code{sd} for \code{"norm"};
#' \code{meanlog} and \code{sdlog} for \code{"lnorm"}; and \code{location}
#' and \code{scale} for \code{"cauchy"}.
#' @param n The total sample size, a whole number between \code{h} and
#' \code{N}.
#' @param N The population size.
#' @param cost A logical: has default cost=FALSE. If it is a stratum-cost
#' problem, cost=TRUE, with which one must provide the Ch parameter.
#' @param ch A numeric: denotes a vector of stratum costs.
#' @param method Character. Optimisation method: \code{"cobyla"} (fast
#' multi-start COBYLA, default), \code{"dp"} (original Dynamic Programming
#' solver), or \code{"global"} (exhaustive global search).
#' @param n_starts Integer. Number of random restarts for the COBYLA solver.
#' Default \code{20L}. Ignored when \code{method = "dp"}.
#' @param max_iter Integer. Maximum function evaluations per COBYLA start.
#' Default \code{2000L}.
#' @param tol Numeric. Convergence tolerance for COBYLA. Default \code{1e-9}.
#' @param verbose Logical. Print per-start progress. Default \code{FALSE}.
#' @return \code{strata.distr} returns Optimum Strata Boundaries (OSB), stratum
#' weights (Wh), stratum costs (Ch), stratum variances (Vh), Optimum Sample
#' Sizes (nh), stratum population sizes (Nh), as an object of class
#' \code{"stratifyR"}. Use \code{\link{boundaries}}, \code{\link{allocation}},
#' \code{\link{objective}} and \code{\link{variance}} to get the results.
#' @author Karuna Reddy <karuna.reddy@@auckland.ac.nz>\cr MGM Khan
#' <mgm.khan@@usp.ac.fj>
#' @seealso \code{\link{strata.data}}, \code{\link{boundaries}},
#' \code{\link{summary.stratifyR}}, \code{\link{compare_designs}}
#' @examples
#' 
#' \dontrun{
#' #Assume data has initial value of 1.5, distance of 33 and follows
#' #weibull distribution with estimated parameters as shape=2.15 and scale=13.5
#' #To compute the OSB, OSS, etc. with fixed sample n=500, we use:
#' res <- strata.distr(h=2, initval=1.5, dist=33, distr = "weibull",
#' params = c(shape=2.15, scale=13.5), n=500, N=2000, cost=FALSE)
#' summary(res)
#' #-------------------------------------------------------------
#' #Assume data has initial value of 1, distance of 10415 and follows
#' #lnorm distribution with estimated parameters as meanlog=5.5 and sdlog=1.5
#' #To compute the OSB, OSS, etc. with fixed sample n=500, we use:
#' res <- strata.distr(h=2, initval=1, dist=10415, distr = "lnorm",
#' params = c(meanlog=5.5, sdlog=1.5), n=500, N=12000)
#' summary(res)
#' #-------------------------------------------------------------
#' #Assume data has initial value of 2, distance of 68 and follows
#' #gamma distribution with estimated parameters as shape=3.8 and rate=0.55
#' #To compute the OSB, OSS, etc. with fixed sample n=500, we use:
#' res <- strata.distr(h=2, initval=0.65, dist=68, distr = "gamma",
#' params = c(shape=3.8, rate=0.55), n=500, N=10000)
#' summary(res)
#' #-------------------------------------------------------------
#' #The function be dynamically used to visualize the the strata boundaries, 
#' #for 2 strata, over the density (or observations) of the "mag" variable 
#' #from the quakes data (with purrr and ggplot2 packages loaded).
#' res <- strata.distr(h=2, initval=4, dist=2.4, distr = "lnorm", 
#'           params = c(meanlog=1.52681032, sdlog=0.08503554), n=300, N=1000)
#' quakes %>% 
#'       ggplot(aes(x = mag)) +
#'       geom_density(fill = "blue", colour = "black", alpha = 0.3) +
#'       geom_vline(xintercept = res$OSB, linetype = "dotted", color = "red")
#' #-------------------------------------------------------------
#' }
#' 
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

  method <- .check_method(method, c("dp", "cobyla", "global"))

  # ---- input checks (v2.1-0) -------------------------------------------------
  if (missing(initval))
    stop("'initval' (the smallest value of the variable) must be given.", call. = FALSE)
  if (missing(dist))
    stop("'dist' (the range of the variable) must be given.", call. = FALSE)
  if (missing(distr))
    stop("'distr' (the name of the distribution) must be given.", call. = FALSE)
  if (missing(params))
    stop("'params' (the parameters of the distribution) must be given.", call. = FALSE)
  if (missing(N) || is.null(N))
    stop("'N' (the population size) must be given.", call. = FALSE)
  .check_h(h)
  distr <- .check_distr(distr, params, initval, dist)
  .check_count(N, "N", min = 2L, what = " (the population size)")
  if (N < 2 * h)
    stop("'N' = ", N, " is too small for ", h, " strata; at least ", 2 * h,
         " units are needed.", call. = FALSE)
  .check_n(n, h, N = N)
  .check_cost(cost, ch, h)
  if (method %in% c("cobyla", "global"))
    .check_solver_settings(n_starts, max_iter, tol, verbose)

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

  # v2.1-0: unrounded stratum values, used by allocation() and objective()
  out5 <- list(stratum_full   = my_env$output_full,
               objective_full = my_env$deno_full)

  out4 <- list(optimality_gap  = optimality_gap,
               kkt_residuals   = kkt_resid,
               converged       = converged,
               method          = method,
               data_internal   = NULL)   # no raw data for distr pathway

  result <- c(out1, out2, out3, out4, out5)
  class(result) <- c("stratifyR", "strata")
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
