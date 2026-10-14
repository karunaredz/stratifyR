# =============================================================================
# cobyla.optim.R
#
# Fast multi-start COBYLA optimiser for univariate stratification.
#
# An alternative to the original Dynamic Programming (DP) solver.  The core
# insight is that both data.root() and distr.root() already return the
# per-stratum contribution Wh * Sh analytically in O(1) time; summing over
# H strata gives the Neyman objective in O(H) time per function evaluation.
# COBYLA then finds the minimum with ~n_starts * max_iter evaluations.
#
# Accuracy: multi-start random restarts guard against local optima; the
# Cauchy-Schwarz lower-bound gap reported in .cobyla_gap() quantifies residual
# suboptimality.
#
# Exports (all internal, prefixed with `.`):
#   .cobyla_obj_data()   - objective for strata.data  (uses data.root)
#   .cobyla_obj_distr()  - objective for strata.distr (uses distr.root)
#   .cobyla_run()        - multi-start driver; returns list(b_dist, obj, converged)
#   .cobyla_gap()        - Cauchy-Schwarz lower bound gap
#   .cobyla_kkt()        - KKT first-order residuals at solution
#   .cobyla_init()       - smart boundary initialisation (quantile + jitter)
#   .cobyla_postprocess()- convert scaled solution back to real-axis statistics
# =============================================================================


# -----------------------------------------------------------------------------
# Objective functions
# -----------------------------------------------------------------------------

# Objective for the data-based pathway - EMPIRICAL version.
#
# Uses the actual data (my_env$data_sorted_real) rather than the analytical
# data.root formula.  The empirical sum(Wh * Sh) is exactly what data.alloc
# reports as WhShTot, so COBYLA and GLOBAL minimise the reported cost
# directly.  (The "dp" solver instead minimises the cost under the fitted
# distribution, and "exact" minimises this same empirical cost by DP on the
# sorted data.)
#
# b_interior: interior boundary distances from initval on the SCALED axis (length H-1).
# Returns sum_h Wh*Sh computed from the raw data.
.cobyla_obj_data <- function(b_interior, my_env, H) {
  # Convert scaled interior distances -> real-axis boundaries
  initval_real <- my_env$initval * my_env$maxval
  b_real <- sort(b_interior) * my_env$maxval + initval_real
  boundaries   <- c(initval_real,
                    b_real,
                    my_env$finval * my_env$maxval)   # length H+1

  data_s <- my_env$data_sorted_real   # pre-sorted raw data (set in .solve_cobyla_data)
  N      <- length(data_s)
  obj    <- 0

  for (h in seq_len(H)) {
    lo <- boundaries[h]
    hi <- boundaries[h + 1L]
    # First stratum is [min, b1]; the others are (b[h-1], b[h]].
    # This matches data.alloc, so every unit is counted exactly once.
    slice <- if (h == 1L)
      data_s[data_s >= lo & data_s <= hi]
    else
      data_s[data_s > lo  & data_s <= hi]

    Nh <- length(slice)
    if (Nh < 2L) return(1e10)          # empty/singleton stratum -> infeasible
    Wh  <- Nh / N
    Sh  <- sqrt(stats::var(slice))
    obj <- obj + Wh * Sh * sqrt(my_env$ch[h])
  }
  obj
}

# Objective for the distribution-based pathway.
# Identical structure but calls distr.root instead of data.root.
.cobyla_obj_distr <- function(b_interior, my_env, H) {
  nb     <- length(b_interior)
  dist   <- my_env$dist
  eps    <- 1e-12 * dist

  b_full <- c(0, sort(b_interior), dist)

  obj <- 0
  for (h in seq_len(H)) {
    d_h <- b_full[h + 1L]
    y_h <- b_full[h + 1L] - b_full[h]
    if (y_h < eps) return(1e10)
    root <- distr.root(d = d_h, y = y_h, c = my_env$ch[h], my_env = my_env)
    if (!is.finite(root) || root == -1) return(1e10)
    obj <- obj + root
  }
  obj
}


# -----------------------------------------------------------------------------
# Initialisation helper
# -----------------------------------------------------------------------------

# Generate starting boundary positions on the scaled axis (distances from initval).
# seed_offset=1 -> equal-spacing (reliable first start).
# seed_offset>1 -> jittered random restarts.
.cobyla_init <- function(H, dist, data_sorted = NULL, seed_offset = 1L) {
  nb  <- H - 1L
  if (nb == 0L) return(numeric(0))
  rng <- dist

  if (seed_offset == 1L) {
    # Equal-spacing (uniform grid on [0, dist])
    b <- rng * seq_len(nb) / H
  } else {
    set.seed(seed_offset * 17L + 3L)
    if (!is.null(data_sorted) && length(data_sorted) >= H) {
      # Quantile-based + jitter (works well for skewed distributions)
      N     <- length(data_sorted)
      alpha <- data_sorted[1L]
      beta  <- data_sorted[N]
      probs <- seq(0, 1, length.out = H + 1L)[2:H]
      q     <- quantile(data_sorted, probs = probs, names = FALSE)
      q_sc  <- (q - alpha) / (beta - alpha) * rng        # scaled
      jit   <- runif(nb, min = -rng / (4 * H), max = rng / (4 * H))
      b     <- q_sc + jit
    } else {
      # No data: uniformly sample the feasible region instead of jittering
      # around equal-spacing. Small jitter fails for skewed distributions where
      # the optimum lies far from the equal-spacing grid.
      min_gap_init <- rng / (H * 3)
      b <- sort(runif(nb, min = min_gap_init, max = rng - min_gap_init))
    }
    b <- pmax(rng * 1e-6, pmin(rng * (1 - 1e-6), b))
    b <- sort(b)
  }

  # Ensure strictly increasing (remove near-duplicate interior boundaries)
  min_gap <- rng / 1000
  for (i in seq_len(nb - 1L)) {
    if (i + 1L <= nb && b[i + 1L] - b[i] < min_gap) {
      b[i + 1L] <- b[i] + min_gap
    }
  }
  b
}


# -----------------------------------------------------------------------------
# Multi-start COBYLA driver
# -----------------------------------------------------------------------------

#' Internal: run multi-start COBYLA for univariate stratification.
#'
#' @param my_env    The shared environment with distribution info, constants.
#' @param H         Number of strata.
#' @param use_distr Logical; TRUE uses distr.root, FALSE uses data.root.
#' @param n_starts  Number of random restarts (default 20).
#' @param max_iter  Max COBYLA function evaluations per start (default 2000).
#' @param tol       Objective relative convergence tolerance (default 1e-9).
#' @param verbose   Print per-start progress.
#' @param data_sorted Optional sorted data for quantile-based initialisation.
#'
#' @return list with:
#'   b_dist    : optimal interior boundaries as distances from initval (length H-1)
#'   objective : achieved objective value (sum Wh*Sh on scaled axis)
#'   converged : logical
.cobyla_run <- function(my_env,
                        H,
                        use_distr   = FALSE,
                        n_starts    = 20L,
                        max_iter    = 2000L,
                        tol         = 1e-9,
                        verbose     = FALSE,
                        data_sorted = NULL) {

  nb   <- H - 1L
  dist <- my_env$dist

  # The random restarts call set.seed() for reproducibility; restore the
  # user's random number stream afterwards so the solver has no side effect.
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    old_seed <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  }

  # H=1 trivial: single stratum spanning the whole range.
  if (nb == 0L) {
    root <- if (use_distr)
      distr.root(d = dist, y = dist, c = my_env$ch[1L], my_env = my_env)
    else
      data.root(d = dist, y = dist, c = my_env$ch[1L], my_env = my_env)
    v <- if (is.finite(root) && root != -1) root else 0
    return(list(b_dist = numeric(0), objective = v, converged = TRUE))
  }

  obj_fn <- if (use_distr) {
    function(b) .cobyla_obj_distr(b, my_env, H)
  } else {
    function(b) .cobyla_obj_data(b, my_env, H)
  }

  # Minimum separation between adjacent boundaries: dist/1000, as in the
  # problem formulation.  Version 2.0-1 used dist/(3H), which excluded the
  # narrow strata that skewed and heavy-tailed populations need.  Designs with
  # fewer than two units in a stratum are handled by the objective (penalty)
  # and by .cobyla_is_degenerate().
  min_gap <- dist / 1000
  lb_vec  <- seq_len(nb) * min_gap                          # i-th boundary >= i*min_gap from start
  ub_vec  <- dist - (nb + 1L - seq_len(nb)) * min_gap      # symmetric from end

  # Inequality constraints: adjacent boundaries must be at least min_gap apart.
  ineq_fn <- if (nb > 1L) {
    function(b) diff(sort(b)) - min_gap
  } else {
    NULL
  }

  # For distribution-based mode, pre-compute equal-Wh quantile start.
  # Equal-probability boundaries are analytically close to the Wh*Sh optimum
  # and provide a much better start than equal-spacing for skewed distributions.
  q_start <- NULL
  if (use_distr && nb >= 1L) {
    q_start <- tryCatch({
      distr_name <- my_env$obj["distr"]
      params     <- my_env$obj[["params"]]
      ymin_real  <- my_env$initval * my_env$maxval
      maxval     <- my_env$maxval
      probs      <- seq(0, 1, length.out = H + 1L)[2L:H]

      q_real <- switch(distr_name,
        weibull = qweibull(probs, shape    = params["shape"], scale   = params["scale"]),
        gamma   = qgamma(  probs, shape    = params["shape"], rate    = params["rate"]),
        exp     = qexp(    probs, rate     = params["rate"]),
        norm    = qnorm(   probs, mean     = params["mean"],  sd      = params["sd"]),
        lnorm   = qlnorm(  probs, meanlog  = params["meanlog"], sdlog = params["sdlog"]),
        unif    = qunif(   probs, min      = params["min"],   max     = params["max"]),
        cauchy  = qcauchy( probs, location = params["location"], scale = params["scale"]),
        NULL
      )

      if (!is.null(q_real) && all(is.finite(q_real))) {
        b_q <- (q_real - ymin_real) / maxval          # -> scaled distance from initval
        b_q <- pmax(min_gap, pmin(dist - min_gap, sort(b_q)))
        b_q
      } else NULL
    }, error = function(e) NULL)
  }

  best_val  <- Inf
  best_b    <- NULL
  best_conv <- FALSE

  for (r in seq_len(n_starts)) {
    # Start selection:
    #  r == 1 -> equal-spacing (always first)
    #  r == 2 -> equal-Wh quantile start (distribution mode) or random
    #  r >= 3 -> .cobyla_init random/uniform starts
    b_init <- if (r == 2L && !is.null(q_start)) {
      q_start
    } else {
      .cobyla_init(H, dist, data_sorted, seed_offset = r)
    }

    # Try BOBYQA first (quadratic model, box constraints only - faster and handles
    # narrow valleys better); fall back to COBYLA if BOBYQA fails or worsens.
    run_one <- function(algo, with_ineq) {
      tryCatch(
        nloptr::nloptr(
          x0          = b_init,
          eval_f      = obj_fn,
          lb          = lb_vec,
          ub          = ub_vec,
          eval_g_ineq = if (with_ineq) ineq_fn else NULL,
          opts = list(
            algorithm = algo,
            xtol_rel  = 1e-8,
            ftol_rel  = tol,
            maxeval   = as.integer(max_iter)
          )
        ),
        error = function(e) NULL
      )
    }
    res_bfq <- run_one("NLOPT_LN_BOBYQA", with_ineq = FALSE)
    res_cob <- run_one("NLOPT_LN_COBYLA", with_ineq = TRUE)
    res <- if (!is.null(res_bfq) && is.finite(res_bfq$objective) &&
               (is.null(res_cob) || !is.finite(res_cob$objective) ||
                res_bfq$objective <= res_cob$objective)) {
      res_bfq
    } else {
      res_cob
    }

    if (!is.null(res) && is.finite(res$objective)) {
      if (verbose)
        cat(sprintf("  [COBYLA] start %2d: obj = %.8f  status = %d\n",
                    r, res$objective, res$status))
      if (res$objective < best_val) {
        best_val  <- res$objective
        best_b    <- sort(res$solution)
        best_conv <- res$status %in% c(1L, 2L, 3L, 4L)
      }
    }
  }

  if (is.null(best_b)) {
    # All starts failed; fall back to equal-spacing
    best_b    <- dist * seq_len(nb) / H
    best_val  <- obj_fn(best_b)
    best_conv <- FALSE
  }

  list(b_dist = best_b, objective = best_val, converged = best_conv)
}


# -----------------------------------------------------------------------------
# Post-processing helpers
# -----------------------------------------------------------------------------

# Convert scaled interior distances back to REAL-axis interior boundary values.
# b_dist: interior distances from initval (scaled).
# Returns OSB: real-axis interior boundaries (length H-1).
.cobyla_real_osb <- function(b_dist, my_env) {
  # b_dist are distances from scaled initval; absolute scaled positions:
  b_scaled_abs <- my_env$initval + sort(b_dist)
  # Real-axis boundaries, followed by the top of the range (v2.0-2: every
  # solver returns h values, the last being the maximum)
  c(b_scaled_abs * my_env$maxval, my_env$finval * my_env$maxval)
}


# Compute per-stratum (Wh, Sh, Wh*Sh) on the SCALED axis for post-processing.
# Returns a data.frame with columns: Wh, Sh, WhSh (all on scaled axis).
.cobyla_stratum_stats <- function(b_dist, my_env, H, use_distr) {
  dist   <- my_env$dist
  b_full <- c(0, sort(b_dist), dist)
  Wh_    <- numeric(H); Sh_ <- numeric(H); WhSh_ <- numeric(H)

  # We back-compute Wh and Sh from the analytic root value and the CDF.
  # Wh = CDF(right) - CDF(left)
  # Sh = WhSh / Wh  (where WhSh = data.root or distr.root return value)
  cdf_fn <- .build_cdf_from_env(my_env, use_distr)

  for (h in seq_len(H)) {
    d_h <- b_full[h + 1L]
    y_h <- b_full[h + 1L] - b_full[h]
    lo  <- my_env$initval + b_full[h]
    hi  <- my_env$initval + b_full[h + 1L]

    Wh_[h]   <- max(cdf_fn(hi) - cdf_fn(lo), 0)
    WhSh_val  <- if (use_distr)
      distr.root(d = d_h, y = y_h, c = 1, my_env = my_env)
    else
      data.root(d = d_h, y = y_h, c = 1, my_env = my_env)

    WhSh_[h] <- if (is.finite(WhSh_val) && WhSh_val != -1) WhSh_val else 0
    Sh_[h]   <- if (Wh_[h] > 1e-12) WhSh_[h] / Wh_[h] else 0
  }

  data.frame(Wh = Wh_, Sh = Sh_, WhSh = WhSh_)
}


# Build a CDF function for the distribution stored in my_env.
# Handles both data-pathway (scaled params) and distr-pathway (divides by maxval).
.build_cdf_from_env <- function(my_env, use_distr = FALSE) {
  distr   <- as.character(my_env$obj["distr"])
  params  <- my_env$obj[["params"]]
  maxval  <- my_env$maxval

  # Scale params if this is the distribution pathway
  if (use_distr) {
    scale_factor <- maxval
  } else {
    scale_factor <- 1   # already fitted to scaled data
  }

  switch(distr,
    weibull = {
      shape <- params["shape"]
      scale <- params["scale"] / scale_factor
      function(x) stats::pweibull(x, shape = shape, scale = scale)
    },
    gamma   = {
      shape <- params["shape"]
      rate  <- params["rate"] * scale_factor
      function(x) stats::pgamma(x, shape = shape, rate = rate)
    },
    exp     = {
      rate  <- params["rate"] * scale_factor
      function(x) stats::pexp(x, rate = rate)
    },
    norm    = {
      mu    <- params["mean"]   / scale_factor
      sigma <- params["sd"]     / scale_factor
      function(x) stats::pnorm(x, mean = mu, sd = sigma)
    },
    lnorm   = {
      mu    <- params["meanlog"] - log(scale_factor)
      sigma <- params["sdlog"]
      function(x) stats::plnorm(x, meanlog = mu, sdlog = sigma)
    },
    cauchy  = {
      loc   <- params["location"] / scale_factor
      sc    <- params["scale"]    / scale_factor
      function(x) stats::pcauchy(x, location = loc, scale = sc)
    },
    unif    = {
      mn    <- params["min"] / scale_factor
      mx    <- params["max"] / scale_factor
      function(x) stats::punif(x, min = mn, max = mx)
    },
    {   # triangle / rtriangle / pareto fallback: use empirical steps
      function(x) {
        n_pts <- 500L
        # approximate CDF via midpoint integration of density
        # (full analytical CDFs for pareto/triangle could be added here)
        x_grid <- seq(my_env$initval, my_env$initval + my_env$dist, length.out = n_pts)
        y_cdf  <- .approx_cdf_from_root(x_grid, my_env, use_distr)
        stats::approx(x_grid, y_cdf, xout = x, rule = 2)$y
      }
    }
  )
}

# Approximate CDF by cumulative sum of per-cell Wh values (fallback for
# distributions without a simple closed-form CDF call above).
.approx_cdf_from_root <- function(x_grid, my_env, use_distr) {
  n   <- length(x_grid)
  cdf <- numeric(n)
  for (i in seq_len(n - 1L)) {
    d_i  <- x_grid[i + 1L] - my_env$initval
    y_i  <- x_grid[i + 1L] - x_grid[i]
    root <- if (use_distr)
      distr.root(d = d_i, y = y_i, c = 1, my_env = my_env)
    else
      data.root(d = d_i, y = y_i, c = 1, my_env = my_env)
    # rough Wh from the ratio WhSh / Sh ~ WhSh / mean_width_stddev
    # for CDF we only need proportional weights; sum and normalise
    cdf[i + 1L] <- cdf[i] + if (is.finite(root) && root != -1) abs(root) else 0
  }
  if (max(cdf) > 0) cdf <- cdf / max(cdf)
  cdf
}


# -----------------------------------------------------------------------------
# Cauchy-Schwarz optimality gap
# -----------------------------------------------------------------------------

# Reports the fractional gap between achieved objective and the Cauchy-Schwarz
# lower bound:  LB = (sum Wh*Sh)^2 / n_total - sum Wh * Sh^2 / N
# gap = (V* - LB) / |LB|.  A gap near 0 indicates near-global optimality.
.cobyla_gap <- function(WhSh_vec, Sh_vec, Wh_vec, n_total, N) {
  deno    <- sum(WhSh_vec)
  V_star  <- deno^2 / n_total - sum(Wh_vec * Sh_vec^2) / N
  lb      <- max(deno^2 / n_total - sum(Wh_vec * Sh_vec^2) / N, 0)
  gap     <- if (lb > 1e-15) (max(V_star, 0) - lb) / abs(lb) else 0
  list(V_star = max(V_star, 0), lb = lb, gap = max(gap, 0))
}


# -----------------------------------------------------------------------------
# KKT first-order residuals
# -----------------------------------------------------------------------------

# At optimum, for each interior boundary b_h:
#   f(b_h) * [W_{h+1}*(mu_{h+1} - b_h)*S_h - W_h*(b_h - mu_h)*S_{h+1}] = 0
# We report the normalised residual for each boundary.  Values near 0 confirm
# first-order optimality; large values indicate a local minimum only.
.cobyla_kkt <- function(b_dist, Wh_vec, Sh_vec, mu_h_vec, my_env) {
  H      <- length(Wh_vec)
  nb     <- H - 1L
  if (nb == 0L) return(numeric(0))

  b_abs  <- my_env$initval + sort(b_dist)   # absolute scaled boundaries
  resid  <- numeric(nb)

  for (i in seq_len(nb)) {
    h   <- i; hp1 <- i + 1L
    lhs <- Wh_vec[hp1] * max(mu_h_vec[hp1] - b_abs[i], 0) * Sh_vec[h]
    rhs <- Wh_vec[h]   * max(b_abs[i] - mu_h_vec[h],   0) * Sh_vec[hp1]
    den <- max(abs(lhs) + abs(rhs), 1e-12)
    resid[i] <- (lhs - rhs) / den
  }
  resid
}


# -----------------------------------------------------------------------------
# Degeneracy detector
# -----------------------------------------------------------------------------

# Returns TRUE only if a stratum is numerically empty (adjacent boundaries
# closer than dist * 1e-6) or, for data, holds fewer than two units.
# In that rare case .solve_cobyla_* fall back to the DP solver.
.cobyla_is_degenerate <- function(b_dist, my_env, h) {
  if (length(b_dist) == 0L) return(FALSE)          # H=1: always valid
  # A solution is degenerate only if a stratum is (numerically) empty.
  # Width alone is not a test: optimal strata for skewed or heavy-tailed
  # populations can be much narrower than the "fair share" dist/H.
  gaps <- diff(c(0, sort(b_dist), my_env$dist))
  if (any(gaps < my_env$dist * 1e-6)) return(TRUE)
  ds <- if (exists("data_sorted_real", envir = my_env, inherits = FALSE))
          get("data_sorted_real", envir = my_env) else NULL
  if (!is.null(ds) && length(ds) > 0L) {
    lo   <- my_env$initval * my_env$maxval
    cuts <- c(-Inf, lo + sort(b_dist) * my_env$maxval, Inf)
    cnt  <- tabulate(findInterval(ds, cuts, left.open = TRUE), nbins = length(cuts) - 1L)
    if (any(cnt < 2L)) return(TRUE)
  }
  FALSE
}


# -----------------------------------------------------------------------------
# Global optimizer: DIRECT-L + BOBYQA refinement
# -----------------------------------------------------------------------------

# Two-phase global optimisation:
#   Phase 1 - DIRECT-L (NLOPT_GN_DIRECT_L): deterministic space-filling search
#             with a budget of max(5000, 5000 * (H - 1)) evaluations.
#   Phase 2 - BOBYQA local refinement from the best DIRECT-L point.
#   If neither phase finds a feasible design (objective below the penalty),
#   the function falls back to multi-start COBYLA (.cobyla_run).
#
# GLOBAL usually matches COBYLA, but the DIRECT-L budget can be too small to
# resolve narrow optimal strata on very wide ranges (e.g. Cauchy, or
# log-normal at larger H).  Multi-start COBYLA is the recommended fast
# solver; "exact" gives the certified optimum for data.
#
# @param my_env          Shared environment (dist, ch, obj, ...).
# @param H               Number of strata.
# @param use_distr       TRUE = distr.root; FALSE = data.root.
# @param max_eval_global Max DIRECT-L evaluations (NULL -> auto from H).
# @param max_eval_local  Max BOBYQA evaluations for the refinement phase.
# @param tol             ftol_rel for the refinement phase.
# @param verbose         Print per-phase progress.
#
# @return list(b_dist, objective, converged) - same layout as .cobyla_run().
.global_run <- function(my_env,
                        H,
                        use_distr        = FALSE,
                        max_eval_global  = NULL,
                        max_eval_local   = 2000L,
                        tol              = 1e-9,
                        verbose          = FALSE) {

  nb   <- H - 1L
  dist <- my_env$dist

  # H = 1: trivial single-stratum case
  if (nb == 0L) {
    root <- if (use_distr)
      distr.root(d = dist, y = dist, c = my_env$ch[1L], my_env = my_env)
    else
      data.root(d = dist, y = dist, c = my_env$ch[1L], my_env = my_env)
    v <- if (is.finite(root) && root != -1) root else 0
    return(list(b_dist = numeric(0), objective = v, converged = TRUE))
  }

  obj_fn <- if (use_distr)
    function(b) .cobyla_obj_distr(b, my_env, H)
  else
    function(b) .cobyla_obj_data(b, my_env, H)

  # Same min-gap constraints as the COBYLA solver
  min_gap <- dist / 1000   # was dist/(3H); see audit
  lb_vec  <- seq_len(nb) * min_gap
  ub_vec  <- dist - (nb + 1L - seq_len(nb)) * min_gap

  # Auto-scale DIRECT-L budget with search-space dimensionality
  if (is.null(max_eval_global))
    max_eval_global <- max(5000L, nb * 5000L)

  # ---- Phase 1: DIRECT-L global search (box constraints only) ---------------
  # DIRECT-L only supports box bounds (no ineq constraints), but our objective
  # internally sorts b before evaluating, so permuted inputs give the same
  # value as their sorted equivalent - DIRECT-L naturally finds sorted solutions.
  x0 <- (lb_vec + ub_vec) / 2

  res_global <- tryCatch(
    nloptr::nloptr(
      x0     = x0,
      eval_f = obj_fn,
      lb     = lb_vec,
      ub     = ub_vec,
      opts   = list(
        algorithm = "NLOPT_GN_DIRECT_L",
        xtol_rel  = 1e-15,   # effectively disabled - rely on maxeval only
        ftol_rel  = 1e-15,   # so DIRECT-L uses its full evaluation budget
        maxeval   = as.integer(max_eval_global)
      )
    ),
    error = function(e) {
      if (verbose) message("[GLOBAL] DIRECT-L failed: ", conditionMessage(e))
      NULL
    }
  )

  if (verbose && !is.null(res_global) && is.finite(res_global$objective))
    cat(sprintf("  [GLOBAL] DIRECT-L : obj = %.8f  (evals = %d)\n",
                res_global$objective, res_global$iterations))

  # ---- Phase 2: BOBYQA local refinement from DIRECT-L solution ---------------
  b_warm <- if (!is.null(res_global) && is.finite(res_global$objective))
    sort(res_global$solution)
  else
    x0

  ineq_fn <- if (nb > 1L) function(b) diff(sort(b)) - min_gap else NULL

  # BOBYQA (quadratic model) handles ridged/narrow-valley objectives better than
  # COBYLA (linear model) when refining from a global search solution.
  # BOBYQA only supports box constraints; the inequality (min-gap) is already
  # satisfied at b_warm (DIRECT-L's solution), so box constraints suffice here.
  res_local <- tryCatch(
    nloptr::nloptr(
      x0     = b_warm,
      eval_f = obj_fn,
      lb     = lb_vec,
      ub     = ub_vec,
      opts   = list(
        algorithm = "NLOPT_LN_BOBYQA",
        xtol_rel  = 1e-10,
        ftol_rel  = tol,
        maxeval   = as.integer(max_eval_local)
      )
    ),
    error = function(e) NULL
  )

  if (verbose && !is.null(res_local) && is.finite(res_local$objective))
    cat(sprintf("  [GLOBAL] BOBYQA   : obj = %.8f\n", res_local$objective))

  # Return the better of the two phases. The objective returns a 1e10 penalty
  # for infeasible partitions (a stratum with fewer than two units), so a
  # penalty value is treated as a failure, not as a solution.
  penalty   <- 1e9
  global_ok <- !is.null(res_global) && is.finite(res_global$objective) &&
               res_global$objective < penalty
  local_ok  <- !is.null(res_local)  && is.finite(res_local$objective) &&
               res_local$objective < penalty
  if (!global_ok && !local_ok) {
    if (verbose) message("[GLOBAL] no feasible point found; using multi-start COBYLA instead.")
    return(.cobyla_run(my_env, H, use_distr = use_distr, tol = tol, verbose = verbose,
                       data_sorted = if (use_distr) NULL else
                         sort(my_env$data_sorted_real / my_env$maxval)))
  }

  # Prefer the refined point on ties: BOBYQA starts at the DIRECT-L point, so an
  # equal objective means the refinement confirmed it (and has a proper
  # convergence status, whereas DIRECT-L always stops on its evaluation budget).
  if (local_ok && (!global_ok || res_local$objective <= res_global$objective)) {
    list(b_dist    = sort(res_local$solution),
         objective = res_local$objective,
         converged = res_local$status %in% c(1L, 2L, 3L, 4L))
  } else if (global_ok) {
    list(b_dist    = sort(res_global$solution),
         objective = res_global$objective,
         converged = FALSE)
  } else {
    list(b_dist    = x0,
         objective = Inf,
         converged = FALSE)
  }
}
