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
    # Spread the cut points evenly in rank, but always keep the first and last
    # few: on heavy-tailed data the optimum can isolate a handful of extreme
    # units in the outer strata, and an even spread would skip those cuts.
    # (v2.0-3) The kept end cuts are spaced geometrically in rank from each
    # end (1, 2, 3, ... then growing by a fixed ratio), so the search sees the
    # far tails in fine detail on both sides, as two-sided heavy tails need.
    L     <- length(cand)
    n_end <- max(4L * h, floor(max_cand / 8))
    geo   <- unique(pmax(1L, round(exp(seq(0, log(max(2, L / 4)), length.out = n_end)))))
    geo   <- sort(unique(c(seq_len(min(4L * h, L)), geo)))
    ends  <- unique(c(geo, L - geo + 1L))
    ends  <- ends[ends >= 1L & ends <= L]
    keep  <- sort(unique(c(ends, round(seq(1, length(cand),
                                           length.out = max_cand - length(ends))))))
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
# =============================================================================
# Exact dynamic programming for the full Neyman variance (v2.0-4)
#
# method = "exact" with fpc = TRUE and/or takeall = TRUE.
#
# Under Neyman allocation of n' units over the sampled strata, the variance of
# the stratified mean, with the finite population correction, is
#
#     V = Phi^2 / n' - Q / N,   Phi = sum W_h S_h,  Q = sum W_h S_h^2,
#
# the sums running over the sampled strata (a take-all stratum adds nothing).
# V is not additive over the strata, but for any lambda > 0
#
#     V >= F_lambda / (lambda N) - n' / (4 lambda^2 N^2),
#     F_lambda = min over partitions of sum (W_h S_h - lambda W_h S_h^2),
#
# and F_lambda is additive, so it is found exactly by the same dynamic
# programme as the objective function. Iterating lambda = n' / (2 Phi N) at the
# current solution gives a partition whose V equals this bound, which
# certifies that it is the minimum of V.
#
# With takeall = TRUE the top stratum may be sampled completely. Every size of
# take-all stratum is tried in the same passes (the prefix optima of the
# dynamic programme are kept for every end point), so each pass gives an upper
# and a lower bound on V for every size; the solver reports the best design and
# the remaining gap between the bounds (zero when certified).
# =============================================================================

.exact_candidates <- function(xs, h, max_cand) {
  Ntot <- length(xs)
  cand <- which(diff(xs) > 0)
  cand <- cand[cand >= 2L & cand <= Ntot - 2L]
  exact_full <- TRUE
  if (length(cand) > max_cand) {
    L     <- length(cand)
    n_end <- max(4L * h, floor(max_cand / 8))
    geo   <- unique(pmax(1L, round(exp(seq(0, log(max(2, L / 4)), length.out = n_end)))))
    geo   <- sort(unique(c(seq_len(min(4L * h, L)), geo)))
    ends  <- unique(c(geo, L - geo + 1L))
    ends  <- ends[ends >= 1L & ends <= L]
    keep  <- sort(unique(c(ends, round(seq(1, length(cand),
                                           length.out = max_cand - length(ends))))))
    cand <- cand[keep]
    exact_full <- FALSE
  }
  list(cand = cand, exact_full = exact_full)
}

# Neyman allocation with take-all capping; returns the variance (with fpc)
.capped_variance <- function(Wh, Sh, Nh, n, force_top = FALSE) {
  L <- length(Nh); take <- rep(FALSE, L); if (force_top) take[L] <- TRUE
  repeat {
    left <- n - sum(Nh[take])
    if (left <= 0 || all(take))            # take-all strata use up the sample
      return(list(V = Inf, nh = ifelse(take, Nh, 0), take = take))
    w    <- ifelse(take, 0, Wh * Sh)
    nh   <- if (sum(w) > 0) ifelse(take, Nh, left * w / sum(w)) else ifelse(take, Nh, left / sum(!take))
    newt <- take | (nh > Nh)
    if (identical(newt, take)) break
    take <- newt
  }
  V <- sum(ifelse(take | Sh == 0, 0, Wh^2 * Sh^2 * (1 / nh - 1 / Nh)))
  # at least two sample units on average in each sampled stratum
  if ((n - sum(Nh[take])) < 2 * sum(!take)) V <- Inf
  list(V = V, nh = nh, take = take)
}

.solve_exact_fpc <- function(data_raw, h, n, N, my_env, takeall = FALSE,
                             max_cand = getOption("stratifyR.exact_max_cand", 20000L),
                             max_iter = 30L) {
  xs   <- sort(as.numeric(data_raw)); Ntot <- length(xs)
  if (Ntot < 2L * h)
    stop("method = \"exact\" needs at least 2 units per stratum (N >= 2h).")
  cc  <- .exact_candidates(xs, h, max_cand)
  pos <- c(0L, cc$cand, Ntot); m <- length(pos)
  ctr <- xs - mean(xs)
  c1  <- c(0, cumsum(ctr)); c2 <- c(0, cumsum(ctr * ctr))

  seg <- function(i_idx, j_idx) {
    a <- pos[i_idx]; b <- pos[j_idx]; nn <- b - a
    s1 <- c1[b + 1L] - c1[a + 1L]; s2 <- c2[b + 1L] - c2[a + 1L]
    v  <- pmax((s2 - s1 * s1 / nn) / pmax(nn - 1, 1), 0)
    list(nn = nn, v = v)
  }
  # one pass of the dynamic programme for sum(W S - lambda W S^2), k = 1..kmax
  dp_pass <- function(lambda, kmax, last_only) {
    cost <- function(i_idx, j_idx) {
      s <- seg(i_idx, j_idx); W <- s$nn / Ntot
      out <- W * sqrt(s$v) - lambda * W * s$v
      out[s$nn < 2] <- Inf
      out
    }
    V   <- matrix(Inf, kmax, m); arg <- matrix(NA_integer_, kmax, m)
    V[1L, 2:m] <- cost(rep(1L, m - 1L), 2:m)
    if (kmax >= 2L) for (k in 2:kmax) {
      js <- if (last_only && k == kmax) m else (k + 1L):m
      for (j in js) {
        i <- k:(j - 1L)
        tot <- V[k - 1L, i] + cost(i, j)
        w <- which.min(tot)
        if (length(w)) { V[k, j] <- tot[w]; arg[k, j] <- i[w] }
      }
    }
    list(V = V, arg = arg)
  }
  trace <- function(arg, k, j) {           # position indices of the stratum ends
    ends <- integer(k); ends[k] <- j
    if (k >= 2L) for (kk in k:2L) { j <- arg[kk, j]; ends[kk - 1L] <- j }
    ends
  }
  stats_of <- function(ends) {             # strata (pos[e_{k-1}], pos[e_k]]
    st <- c(1L, ends)
    s  <- seg(st[-length(st)], st[-1])
    list(Nh = s$nn, Wh = s$nn / Ntot, Sh = sqrt(s$v), v = s$v)
  }
  Vfull <- function(s, nprime) {
    phi <- sum(s$Wh * s$Sh)
    list(V = phi^2 / nprime - sum(s$Wh * s$v) / Ntot, phi = phi)
  }

  # ---- (A) all strata sampled: exact minimum of the full variance ----------
  P0  <- dp_pass(0, h, TRUE); ends <- trace(P0$arg, h, m)
  sA  <- stats_of(ends); fA <- Vfull(sA, n)
  lam <- n / (2 * fA$phi * Ntot); bestA <- list(ends = ends, V = fA$V); LB_A <- -Inf
  for (it in seq_len(max_iter)) {
    P  <- dp_pass(lam, h, TRUE); e2 <- trace(P$arg, h, m)
    s2 <- stats_of(e2); f2 <- Vfull(s2, n)
    LB_A <- max(LB_A, P$V[h, m] / (lam * Ntot) - n / (4 * lam^2 * Ntot^2))
    if (f2$V < bestA$V) bestA <- list(ends = e2, V = f2$V)
    newlam <- n / (2 * f2$phi * Ntot)
    if (identical(e2, ends) && abs(newlam - lam) <= 1e-12 * lam) break
    ends <- e2; lam <- newlam
  }
  # not certified: sweep lambda over a wide range (the minimum of V need not be
  # reached by the iteration when the bound is not tight)
  sA0 <- stats_of(bestA$ends)
  feasA0 <- all(n * sA0$Wh * sA0$Sh / sum(sA0$Wh * sA0$Sh) <= sA0$Nh)
  if (feasA0 && (bestA$V - LB_A) > 1e-8 * abs(bestA$V)) {
    lam_c <- n / (2 * sum(stats_of(bestA$ends)$Wh * stats_of(bestA$ends)$Sh) * Ntot)
    for (f in exp(seq(log(0.1), log(10), length.out = 9))) {
      P  <- dp_pass(lam_c * f, h, TRUE); e2 <- trace(P$arg, h, m)
      f2 <- Vfull(stats_of(e2), n)
      LB_A <- max(LB_A, P$V[h, m] / (lam_c * f * Ntot) - n / (4 * (lam_c * f)^2 * Ntot^2))
      if (f2$V < bestA$V) bestA <- list(ends = e2, V = f2$V)
    }
  }
  sA  <- stats_of(bestA$ends)
  capA <- .capped_variance(sA$Wh, sA$Sh, sA$Nh, n)
  choice <- list(ends = bestA$ends, takeall = 0L, V = capA$V)

  # ---- (B) take-all top stratum: try every size in the same passes ---------
  if (isTRUE(takeall) && h >= 2L) {
    NT   <- Ntot - pos                     # take-all size if the sampled part ends at pos[j]
    J    <- which(NT >= 1L & NT <= n - 2L * (h - 1L) & pos >= 2L * (h - 1L))
    if (length(J)) {
      UB <- rep(Inf, m); LB <- rep(-Inf, m); bestE <- vector("list", m)
      run <- function(lam) {
        P <- dp_pass(lam, h - 1L, FALSE)
        for (j in J) {
          if (!is.finite(P$V[h - 1L, j])) next
          np <- n - NT[j]
          e  <- trace(P$arg, h - 1L, j); s <- stats_of(c(e, m))
          cv <- .capped_variance(s$Wh, s$Sh, s$Nh, n, force_top = TRUE)$V
          if (cv < UB[j]) { UB[j] <<- cv; bestE[[j]] <<- e }
          LB[j] <<- max(LB[j], P$V[h - 1L, j] / (lam * Ntot) - np / (4 * lam^2 * Ntot^2))
        }
      }
      lam0 <- n / (2 * sum(sA$Wh * sA$Sh) * Ntot)
      for (f in c(0.5, 1, 2)) run(lam0 * f)
      lam_prev <- NA_real_
      for (it in seq_len(min(max_iter, 10L))) {
        if (!any(is.finite(UB[J]))) break
        jb  <- J[which.min(UB[J])]; s <- stats_of(bestE[[jb]])
        lam <- (n - NT[jb]) / (2 * sum(s$Wh * s$Sh) * Ntot)
        if (!is.na(lam_prev) && abs(lam - lam_prev) <= 1e-10 * lam) break
        run(lam); lam_prev <- lam
      }
      if (any(is.finite(UB[J]))) {
        jb <- J[which.min(UB[J])]
        eB <- c(bestE[[jb]], m); sB <- stats_of(eB)
        capB <- .capped_variance(sB$Wh, sB$Sh, sB$Nh, n, force_top = TRUE)
        if (capB$V < choice$V)
          choice <- list(ends = eB, takeall = as.integer(NT[jb]), V = capB$V)
      }
      LB_all <- min(LB_A, min(LB[J]))
    } else LB_all <- LB_A
  } else LB_all <- LB_A
  # certificate: no design with all strata sampled, or with a take-all top
  # stratum (when takeall = TRUE), has a lower variance than LB_all
  # The bound is tight only when the Neyman allocation is feasible with every
  # stratum sampled; the gap is reported (0 = certified optimum) in that case.
  sC <- stats_of(choice$ends)
  feasC <- choice$takeall == 0L &&
    all(n * sC$Wh * sC$Sh / sum(sC$Wh * sC$Sh) <= sC$Nh)
  choice$gap <- if (feasC && is.finite(choice$V))
    max(0, (Vfull(sC, n)$V - LB_all) / Vfull(sC, n)$V) else NA_real_

  # ---- outputs in the same form as .solve_exact_data() ----------------------
  cuts   <- pos[choice$ends[-length(choice$ends)]]
  b_real <- xs[cuts]
  b_mid  <- (xs[cuts] + xs[cuts + 1L]) / 2
  OSB <- c(b_real, max(xs))
  my_env$df <- data.frame(h = seq_len(h), d = NA_real_, y = NA_real_,
                          x = c(b_mid / my_env$maxval, my_env$finval))
  my_env$takeall_size <- choice$takeall
  attr(OSB, "converged")      <- TRUE
  attr(OSB, "optimality_gap") <- choice$gap
  attr(OSB, "kkt_residuals")  <- rep(NA_real_, h - 1L)
  attr(OSB, "exact_full")     <- cc$exact_full
  attr(OSB, "n_candidates")   <- length(cc$cand)
  attr(OSB, "takeall")        <- choice$takeall
  OSB
}
