# =============================================================================
# exact.optim.R  -  stratifyR 2.0-2
#
# Exact dynamic programming on the data (method = "exact").
#
# The "dp" solver is globally optimal on a grid for the FITTED distribution.
# This solver is globally optimal for the DATA themselves: it minimises the
# empirical Neyman cost
#
#     Phi = sum_h W_h S_h sqrt(c_h),   W_h = N_h / N,  S_h = sd of stratum h,
#
# over every partition of the sorted values into h contiguous strata with at
# least two units each. Boundaries can only fall between two distinct values,
# so ties are never split. Prefix sums make each stratum cost O(1), and the
# Bellman recursion is vectorised over the candidate start points, giving
# O(h * m^2) work for m candidate cut points.
#
# When the data have more than `max_cand` (default 20,000) distinct cut points the search is
# run over `max_cand` cut points spread evenly in rank (quantiles); the result
# is then exact on that candidate set and the object records this.
# =============================================================================

.solve_exact_data <- function(data_raw, h, n, N, my_env,
                              max_cand = getOption("stratifyR.exact_max_cand", 20000L)) {
  xs  <- sort(as.numeric(data_raw))
  Ntot <- length(xs)
  if (Ntot < 2L * h)
    stop("method = \"exact\" needs at least 2 units per stratum (N >= 2h).")

  # candidate cut positions j: the lower stratum ends at xs[j]; need xs[j] < xs[j+1]
  cand <- which(diff(xs) > 0)
  cand <- cand[cand >= 2L & cand <= Ntot - 2L]
  exact_full <- TRUE
  if (length(cand) > max_cand) {
    keep <- unique(round(seq(1, length(cand), length.out = max_cand)))
    cand <- cand[keep]
    exact_full <- FALSE
  }
  if (length(cand) < h - 1L)
    stop("Not enough distinct values to form ", h, " strata.")

  pos <- c(0L, cand, Ntot)                 # split positions incl. both ends
  m   <- length(pos)
  ctr <- xs - mean(xs)                     # centre for numerical stability
  c1  <- c(0, cumsum(ctr)); c2 <- c(0, cumsum(ctr * ctr))
  ch  <- my_env$ch

  # cost of strata (pos[i], pos[j]] for a vector of i and one j, stratum index k
  seg_cost <- function(i_idx, j_idx, k) {
    a  <- pos[i_idx]; b <- pos[j_idx]; nn <- b - a
    s1 <- c1[b + 1L] - c1[a + 1L]; s2 <- c2[b + 1L] - c2[a + 1L]
    v  <- pmax((s2 - s1 * s1 / nn) / pmax(nn - 1, 1), 0)
    out <- (nn / Ntot) * sqrt(v) * sqrt(ch[k])
    out[nn < 2] <- Inf
    out
  }

  V   <- matrix(Inf, nrow = h, ncol = m)   # V[k, j]: best cost of first k strata ending at pos[j]
  arg <- matrix(NA_integer_, nrow = h, ncol = m)
  for (j in 2:m) V[1L, j] <- seg_cost(1L, j, 1L)

  if (h >= 2L) for (k in 2:h) {
    js <- if (k < h) (k + 1L):(m - 1L) else m
    for (j in js) {
      i <- k:(j - 1L)                      # previous stratum ends at pos[i]
      tot <- V[k - 1L, i] + seg_cost(i, j, k)
      w <- which.min(tot)
      if (length(w)) { V[k, j] <- tot[w]; arg[k, j] <- i[w] }
    }
  }
  if (!is.finite(V[h, m]))
    stop("No feasible partition with at least two units per stratum.")

  # trace back the split positions
  cuts <- integer(h - 1L); j <- m
  for (k in h:2L) { j <- arg[k, j]; cuts[k - 1L] <- pos[j] }
  b_real <- xs[cuts]                       # boundary = largest value in the lower stratum

  # For the allocation step, store a cut point halfway to the next value rather
  # than the data value itself. data.alloc() rebuilds boundaries as
  # (b / maxval) * maxval, and a one-ulp rounding difference (which varies by
  # platform, e.g. with fused multiply-add on Apple silicon) would otherwise move
  # a unit equal to the boundary into the next stratum.
  b_mid <- (xs[cuts] + xs[cuts + 1L]) / 2

  OSB <- c(b_real, max(xs))
  my_env$df <- data.frame(h = seq_len(h), d = NA_real_, y = NA_real_,
                          x = c(b_mid / my_env$maxval, my_env$finval))
  attr(OSB, "converged")      <- TRUE
  attr(OSB, "optimality_gap") <- 0
  attr(OSB, "kkt_residuals")  <- rep(NA_real_, h - 1L)
  attr(OSB, "exact_full")     <- exact_full
  attr(OSB, "n_candidates")   <- length(cand)
  OSB
}
