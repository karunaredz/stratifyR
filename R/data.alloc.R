#' Allocate data
#' To calculate the stratum sample sizes (nh) for a fixed sample size (n) 
#' directly based on the data
#'
#' @param data Input dataset.
#' @param my_env Environment object.
#' @return ...
#'
#' Uses OSB to compute stratum weights (Wh), sample variances (Vh from data),
#' and Neyman allocations (nh). Builds the summary table once at the end.
data.alloc <- function(data, my_env)
{
  # -------- unpack inputs / setup --------
  h       <- my_env$h
  initval <- my_env$initval * my_env$maxval          # back to real (unscaled) units
  n       <- my_env$n
  x       <- c(initval, my_env$df$x * my_env$maxval) # OSB (including left edge) on real scale
  ch      <- my_env$ch                               # per-stratum unit costs
  
  # preallocate result vectors (faster than growing them)
  var  <- numeric(h)  # stratum sample variances from raw data
  Wh   <- numeric(h)  # stratum weights from raw data
  whsh <- numeric(h)  # pure Neyman contribution Wh * Sh (reported, cost-free)
  nume <- numeric(h)  # cost-optimal allocation weight Wh * Sh / sqrt(cost)
  nh   <- numeric(h)  # sample sizes per stratum
  Nh   <- numeric(h)  # population counts per stratum
  fh   <- numeric(h)  # sampling fractions per stratum
  deno <- 0           # sum of pure Wh*Sh across strata (the Neyman cost)
  adeno <- 0          # sum of cost-adjusted allocation weights
  
  # -------- compute Wh, Vh, Nh from data --------
  data <- sort(data)
  Ntot <- length(data)
  
  # v2.0-2: assign every unit to exactly one stratum, (b_{i-1}, b_i], with the
  # first stratum closed below and the last open above, so no unit is dropped
  # or counted twice.
  grp <- findInterval(data, x[2:h], left.open = TRUE) + 1L
  for (i in 1:(length(x) - 1L)) {
    slice   <- data[grp == i]
    Nh[i]   <- length(slice)
    Wh[i]   <- Nh[i] / Ntot
    # Guard: var() returns NA for 0- or 1-element slices; treat as 0
    var[i]  <- if (length(slice) >= 2L) stats::var(slice) else 0
    whsh[i] <- Wh[i] * sqrt(var[i])            # pure Neyman contribution
    nume[i] <- whsh[i] / sqrt(ch[i])           # cost-optimal allocation weight
    deno    <- deno + whsh[i]
    adeno   <- adeno + nume[i]
  }
  
  # -------- build summary table ONCE (moved out of the loop) --------
  my_env$output <- data.frame(
    Wh   = round(Wh,  2),
    Vh   = round(var, 2),
    WhSh = round(whsh, 3)
  )
  my_env$Nh <- Nh
  
  # -------- initial Neyman allocation (sum to n) --------
  # Guard: if deno is zero or NA (all strata empty/singleton), fall back to equal allocation
  if (is.na(adeno) || adeno <= 0) adeno <- 1
  for (i in 1:(length(x) - 1L)) {
    nh[i] <- n * nume[i] / adeno
  }
  
  # -------- take-all top stratum (method = "exact", takeall = TRUE) --------
  # v2.0-3: the top stratum is sampled completely and the rest of the sample is
  # allocated over the other strata in the same way as above.
  ta <- isTRUE(my_env$takeall_size > 0) && h >= 2L && Nh[h] < n
  if (ta) {
    rest <- seq_len(h - 1L)
    nh[h] <- Nh[h]
    nh[rest] <- (n - Nh[h]) * nume[rest] / sum(nume[rest])
  }

  # -------- fix oversampling if any nh > Nh (your existing logic) --------
  realloc(h, x, nh, Nh, nume, my_env)
  nh <- my_env$nh
  
  for (i in 1:(length(x) - 1L)) {
    if (nh[i] > Nh[i]) {
      realloc(h, x, nh, Nh, nume, my_env)
      nh <- my_env$nh
    } else {
      nh <- my_env$nh
    }
  }
  
  # -------- assemble user-facing outputs and totals --------
  # v2.0-2: whole-number sample sizes that add up to n (largest remainder)
  nh <- .round_alloc(nh, Nh)
  fh <- ifelse(Nh == 0, 0, round(nh / Nh, 2))   # guard: 0/0 -> 0 for empty strata

  # v2.0-3: variance of the stratified mean (with fpc) for this allocation, and
  # a warning when a sampled stratum gets fewer than 2 units
  samp <- nh > 0 & nh < Nh
  my_env$variance <- sum((Wh^2 * var * (1 / nh - 1 / Nh))[samp])
  if (any(nh < 2 & Nh >= 2))
    warning("At least one stratum has fewer than 2 sample units, so its variance ",
            "cannot be estimated from the sample. Increase n or reduce h.",
            call. = FALSE)
  my_env$out <- data.frame(nh = nh, Nh = Nh, fh = fh)
  
  my_env$deno  <- round(deno, 6)
  my_env$WhTot <- round(sum(Wh), 0)
  my_env$NhTot <- sum(Nh)
  my_env$nhTot <- round(sum(nh))
  my_env$VhTot <- round(sum(var), 2)
  my_env$fhTot <- round(my_env$nhTot / my_env$NhTot, 2)
}