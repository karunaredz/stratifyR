# =============================================================================
# plot.strata.R  --  stratifyR 2.0
# =============================================================================

#' Plot Method for Stratified Survey Design Objects
#'
#' @param x       A \code{"strata"} object.
#' @param type    \code{"2d"} (default), \code{"3d"}, or \code{"interactive"}.
#' @param data    Optional numeric vector of population values.
#' @param n_pts   Integer. Density grid size. Default \code{512L}.
#' @param alpha   Numeric. Stratum fill transparency. Default \code{0.25}.
#' @param palette Character vector of stratum colours (recycled).
#' @param main    Character. Plot title (auto-generated if \code{NULL}).
#' @param ...     Passed to \code{hist()} or \code{plotly::layout()}.
#'
#' @return Invisibly returns \code{x} for \code{"2d"}; a plotly widget
#'   for \code{"3d"} / \code{"interactive"}.
#'
#' @examples
#' \dontrun{
#' set.seed(1); y <- rgamma(2000, shape = 2, rate = 0.5)
#' res <- strata.data(y, h = 4, n = 400)
#' plot(res)
#' plot(res, type = "3d")
#' plot(res, type = "interactive")
#' }
#'
#' @export
plot.strata <- function(x,
                         type      = c("2d", "3d", "interactive"),
                         data      = NULL,
                         n_pts     = 512L,
                         alpha     = 0.25,
                         palette   = c("#4E79A7", "#F28E2B", "#E15759",
                                       "#76B7B2", "#59A14F", "#EDC948",
                                       "#B07AA1", "#FF9DA7", "#9C755F"),
                         main      = NULL,
                         show_info = TRUE,
                         ...) {
  type     <- match.arg(type)
  pop_data <- data %||% x$data_internal
  H        <- nrow(x$h)

  switch(type,
    "2d"          = .plot_2d(x, pop_data, H, n_pts, alpha, palette, main, show_info, ...),
    "3d"          = .plot_3d(x, pop_data, H, n_pts, alpha, palette, main, ...),
    "interactive" = .plot_interactive(x, pop_data, H, n_pts, alpha, palette, main, ...)
  )
}


# =============================================================================
# 2D plot  (base R)  --  two-panel layout: plot (left) | info panel (right)
# =============================================================================
.plot_2d <- function(x, pop_data, H, n_pts, alpha, palette, main, show_info = TRUE, ...) {
  OSB    <- x$OSB
  distr  <- x$distr
  maxval <- x$maxval

  .xlo2d <- maxval * x$initval
  .xhi2d <- max(maxval * x$finval, max(OSB))
  full_B <- c(.xlo2d, OSB[OSB > .xlo2d & OSB < .xhi2d], .xhi2d)
  cols   <- rep_len(palette, H)

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)

  if (show_info) {
    # Two-column layout: 3 parts plot, 1.5 parts info panel
    graphics::layout(matrix(c(1L, 2L), 1L, 2L), widths = c(3.0, 1.5))
  }

  # ===========================================================================
  # Panel 1 -- Stratification plot
  # ===========================================================================
  par(mar = if (show_info) c(5, 4.5, 4, 0.5) else c(4, 4.5, 3.5, 1))

  if (!is.null(pop_data) && length(pop_data) >= 10L) {
    h_obj   <- hist(pop_data, plot = FALSE)
    x_range <- range(pop_data)
    y_max   <- max(h_obj$density) * 1.35
    hist(pop_data, freq = FALSE, col = "grey90", border = "white",
         xlim = x_range, ylim = c(0, y_max),
         xlab = "Y", ylab = "Density",
         main = main %||% sprintf(
           "Optimal Stratification  (%d Strata, n = %d, distr = %s)",
           H, x$n, distr), ...)
  } else {
    x_lo  <- maxval * x$initval
    x_hi  <- maxval * x$finval
    x_tmp <- seq(x_lo, x_hi, length.out = n_pts)
    d_tmp <- .eval_density(x_tmp, x, use_distr = TRUE)
    plot(x_tmp, d_tmp, type = "n",
         xlab = "Y", ylab = "Density",
         main = main %||% sprintf(
           "Optimal Stratification  (%d Strata, n = %d, distr = %s)",
           H, x$n, distr), ...)
  }

  x_lo  <- maxval * x$initval
  x_hi  <- maxval * x$finval
  x_seq <- seq(x_lo, x_hi, length.out = n_pts)
  dens  <- .eval_density(x_seq, x, use_distr = is.null(pop_data))
  dens[!is.finite(dens)] <- 0

  for (h in seq_len(H)) {
    lo_h <- full_B[h]; hi_h <- full_B[h + 1L]
    idx  <- x_seq >= lo_h & x_seq <= hi_h
    if (sum(idx) < 2L) next
    polygon(c(x_seq[idx], rev(x_seq[idx])),
            c(dens[idx], rep(0, sum(idx))),
            col = grDevices::adjustcolor(cols[h], alpha.f = alpha), border = NA)
  }

  lines(x_seq, dens, col = "#1a1a2e", lwd = 2.2)
  abline(v = OSB, col = "#C0392B", lty = 2, lwd = 1.8)

  # OSB labels: right of each dashed line, near top of plot
  usr   <- par("usr")
  x_rng <- usr[2] - usr[1]
  y_lbl <- usr[4] - 0.04 * (usr[4] - usr[3])

  for (i in seq_along(OSB)) {
    spc <- (usr[2] - OSB[i]) / x_rng
    if (spc > 0.12) {
      xp  <- OSB[i] + 0.012 * x_rng; adj <- c(0, 0.5)
    } else {
      xp  <- OSB[i] - 0.012 * x_rng; adj <- c(1, 0.5)
    }
    text(xp, y_lbl,
         labels = as.expression(bquote(b[.(i)] == .(round(OSB[i], 3)))),
         col = "#C0392B", cex = 0.80, adj = adj)
  }

  if (!show_info) return(invisible(x))

  # ===========================================================================
  # Panel 2 -- Info panel (right side)
  # ===========================================================================
  par(mar = c(5, 0.2, 4, 1.5))
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))

  # Collect parameters
  params <- tryCatch({
    fit <- x$fit
    if (!is.null(fit$estimate))          fit$estimate
    else if (!is.null(fit$fit$estimate)) fit$fit$estimate
    else NULL
  }, error = function(e) NULL)
  n_params <- if (!is.null(params)) length(params) else 0L

  # Per-distribution Greek symbol map (Unicode, for parameter value labels)
  greek_map <- switch(distr,
    norm      = c(mean = "\u03bc",   sd      = "\u03c3"),
    lnorm     = c(meanlog = "\u03bc", sdlog  = "\u03c3"),
    gamma     = c(shape = "\u03b1",  rate    = "\u03b2"),
    weibull   = c(shape = "k",       scale   = "\u03bb"),
    exp       = c(rate  = "\u03bb"),
    cauchy    = c(location = "x\u2080", scale = "\u03b3"),
    unif      = c(min = "a",         max     = "b"),
    NULL
  )

  # -- Outer box --------------------------------------------------------------
  rect(0.03, 0.02, 0.97, 0.98,
       col = "#EEF2FF", border = "#7090CC", lwd = 1.5)

  # -- Distribution name (top) ------------------------------------------------
  text(0.50, 0.937, .dist_display_name(distr),
       col = "#1a1a6e", font = 2, cex = 0.86, adj = c(0.5, 0.5))
  lines(c(0.07, 0.93), c(0.895, 0.895), col = "#7090CC", lwd = 0.9)

  # -- Symbolic PDF equation (plotmath Greek letters built-in) ----------------
  text(0.50, 0.815,
       labels = as.expression(.make_density_eqn_symbolic(distr)),
       cex = 0.74, adj = c(0.5, 0.5))
  lines(c(0.10, 0.90), c(0.757, 0.757), col = "#9ab0CC", lwd = 0.5, lty = 3)

  # -- Parameter values -------------------------------------------------------
  text(0.50, 0.725, "Parameters",
       col = "#555555", cex = 0.73, font = 3L, adj = c(0.5, 0.5))

  p_step    <- min(0.080, 0.22 / max(n_params, 1L))
  y_p_first <- 0.672

  if (!is.null(params) && n_params > 0L) {
    for (pi_i in seq_len(n_params)) {
      nm  <- names(params)[pi_i]
      val <- as.numeric(params[pi_i])
      sym <- if (!is.null(greek_map) && nm %in% names(greek_map)) greek_map[[nm]] else nm
      text(0.50, y_p_first - (pi_i - 1L) * p_step,
           sprintf("%s (%s) = %.4f", sym, nm, val),
           col = "#222222", cex = 0.76, adj = c(0.5, 0.5))
    }
  }

  y_sep2   <- y_p_first - n_params * p_step - 0.018
  y_ss_hdr <- y_sep2 - 0.048
  lines(c(0.07, 0.93), c(y_sep2, y_sep2), col = "#7090CC", lwd = 0.9)

  # -- Strata Summary ---------------------------------------------------------
  text(0.50, y_ss_hdr, "Strata Summary",
       col = "#444444", cex = 0.74, font = 3L, adj = c(0.5, 0.5))

  Wh_v <- round(x$Wh, 4)
  Sh_v <- round(sqrt(pmax(x$Vh, 0)), 4)
  nh_v <- x$nh

  y_strata_top <- y_ss_hdr - 0.048
  blk_h        <- min(0.115, (y_strata_top - 0.05) / H)

  for (hi in seq_len(H)) {
    y_blk <- y_strata_top - (hi - 1L) * blk_h
    y_top <- y_blk - 0.006
    y_bot <- y_blk - 0.038

    # Colour swatch
    rect(0.06, y_top - 0.004, 0.10, y_top + 0.020,
         col = cols[hi], border = NA)

    # "Stratum N  [lo -- hi]"  (one line, bold)
    bnd_lo <- round(full_B[hi],       3)
    bnd_hi <- round(full_B[hi + 1L],  3)
    text(0.54, y_top + 0.006,
         sprintf("Stratum %d   [%.3f - %.3f]", hi, bnd_lo, bnd_hi),
         col = cols[hi], cex = 0.75, adj = c(0.5, 0.5), font = 2)

    # Wh, Sh, nh on second line
    text(0.54, y_bot + 0.007,
         sprintf("Wh = %.4f     Sh = %.4f     nh = %d",
                 Wh_v[hi], Sh_v[hi], nh_v[hi]),
         col = cols[hi], cex = 0.70, adj = c(0.5, 0.5))
  }

  invisible(x)
}


# =============================================================================
# Distribution label helpers
# =============================================================================

.dist_display_name <- function(distr) {
  switch(distr,
    norm      = "Normal Distribution",
    lnorm     = "Log-Normal Distribution",
    gamma     = "Gamma Distribution",
    weibull   = "Weibull Distribution",
    exp       = "Exponential Distribution",
    cauchy    = "Cauchy Distribution",
    unif      = "Uniform Distribution",
    pareto    = "Pareto Distribution",
    triangle  = "Triangular Distribution",
    rtriangle = "Right-Triangular Distribution",
    paste(toupper(substring(distr, 1, 1)),
          substring(distr, 2), " Distribution", sep = "")
  )
}

# Returns a character string using Unicode Greek letters for base-R mtext
.greek_param_str <- function(distr, params) {
  if (is.null(params) || length(params) == 0L) return("")

  greek <- switch(distr,
    norm      = c(mean = "\u03bc",  sd      = "\u03c3"),
    lnorm     = c(meanlog = "\u03bc", sdlog = "\u03c3"),
    gamma     = c(shape = "\u03b1", rate    = "\u03b2"),
    weibull   = c(shape = "k",      scale   = "\u03bb"),
    exp       = c(rate  = "\u03bb"),
    cauchy    = c(location = "x\u2080", scale = "\u03b3"),
    unif      = c(min = "a",        max    = "b"),
    NULL
  )

  parts <- mapply(function(nm, val) {
    sym <- if (!is.null(greek) && nm %in% names(greek)) greek[[nm]] else nm
    sprintf("%s (%s) = %.4f", sym, nm, val)
  }, names(params), as.numeric(params), SIMPLIFY = TRUE)

  paste(parts, collapse = "   ,   ")
}

# Returns HTML string using HTML entity Greek letters for plotly
.greek_param_html <- function(distr, params, sep = "   ,   ") {
  if (is.null(params) || length(params) == 0L) return("")

  greek <- switch(distr,
    norm      = c(mean = "\u03bc",     sd      = "\u03c3"),
    lnorm     = c(meanlog = "\u03bc",  sdlog   = "\u03c3"),
    gamma     = c(shape = "\u03b1", rate    = "\u03b2"),
    weibull   = c(shape = "k",       scale   = "\u03bb"),
    exp       = c(rate  = "\u03bb"),
    cauchy    = c(location = "x<sub>0</sub>", scale = "\u03b3"),
    unif      = c(min = "a",         max     = "b"),
    NULL
  )

  parts <- mapply(function(nm, val) {
    sym <- if (!is.null(greek) && nm %in% names(greek)) greek[[nm]] else nm
    sprintf("%s (%s) = %.4f", sym, nm, val)
  }, names(params), as.numeric(params), SIMPLIFY = TRUE)

  paste(parts, collapse = sep)
}


# =============================================================================
# Density equation (plotmath) for 2D base-R plot
# =============================================================================
.make_density_eqn <- function(distr, params) {
  if (is.null(params) || length(params) == 0L)
    return(bquote(f(y) ~ "[" * .(distr) ~ "distribution]"))

  p <- params
  g <- function(nm, d = 3) {
    v <- tryCatch(round(as.numeric(p[nm]), d), error = function(e) NA_real_)
    if (length(v) == 0L || is.na(v)) NA_real_ else v
  }

  tryCatch(switch(distr,

    norm = {
      mu <- g("mean"); sg <- g("sd")
      bquote(
        f(y) == frac(1, .(sg) %.% sqrt(2*pi)) %.%
          e^{-(y - .(mu))^2 / (2 %.% .(sg)^2)}
      )
    },

    lnorm = {
      ml <- g("meanlog"); sl <- g("sdlog")
      bquote(
        f(y) == frac(1, y %.% .(sl) %.% sqrt(2*pi)) %.%
          e^{-(log(y) - .(ml))^2 / (2 %.% .(sl)^2)}
      )
    },

    gamma = {
      sh <- g("shape"); rt <- g("rate"); sh1 <- round(sh - 1, 3)
      bquote(
        f(y) == frac(.(rt)^.(sh), Gamma(.(sh))) %.%
          y^.(sh1) %.% e^{-.(rt) %.% y}
      )
    },

    weibull = {
      sh <- g("shape"); sc <- g("scale"); sh1 <- round(sh - 1, 3)
      bquote(
        f(y) == frac(.(sh), .(sc)) %.%
          bgroup("(", frac(y, .(sc)), ")")^.(sh1) %.%
          e^{-(y / .(sc))^.(sh)}
      )
    },

    exp = {
      rt <- g("rate")
      bquote(f(y) == .(rt) %.% e^{-.(rt) %.% y})
    },

    cauchy = {
      loc <- g("location"); sc <- g("scale")
      bquote(
        f(y) == frac(1,
          pi %.% .(sc) %.%
          bgroup("[", 1 + bgroup("(", frac(y - .(loc), .(sc)), ")")^2, "]"))
      )
    },

    unif = {
      mn <- g("min"); mx <- g("max")
      bquote(f(y) == frac(1, .(mx) - .(mn)) ~~ "for" ~~ .(mn) <= y ~ "" <= .(mx))
    },

    bquote(f(y) ~ "[" * .(distr) ~ "distribution]")

  ), error = function(e) {
    bquote(f(y) ~ "[" * .(distr) ~ "distribution]")
  })
}


# =============================================================================
# Symbolic density equations for the right-side info panel
# Uses plotmath Greek keywords (sigma, mu, lambda, alpha, beta, gamma, pi)
# which R renders automatically as Greek letters -- no Unicode needed.
# =============================================================================
.make_density_eqn_symbolic <- function(distr) {
  switch(distr,

    norm    = bquote(
      f(y) == frac(1, sigma * sqrt(2 * pi)) * e^{-(y - mu)^2 / (2 * sigma^2)}
    ),

    lnorm   = bquote(
      f(y) == frac(1, y * sigma * sqrt(2 * pi)) * e^{-(log(y) - mu)^2 / (2 * sigma^2)}
    ),

    gamma   = bquote(
      f(y) == frac(beta^alpha, Gamma(alpha)) * y^{alpha - 1} * e^{-beta * y}
    ),

    weibull = bquote(
      f(y) == frac(k, lambda) * (y / lambda)^{k - 1} * e^{-(y / lambda)^k}
    ),

    exp     = bquote(
      f(y) == lambda * e^{-lambda * y}
    ),

    cauchy  = bquote(
      f(y) == frac(1, pi * gamma * (1 + ((y - x[0]) / gamma)^2))
    ),

    unif    = bquote(
      f(y) == frac(1, b - a) ~~ "," ~~ a <= y ~ "" <= b
    ),

    bquote(f(y) ~ "[" * .(distr) * "]")
  )
}


# =============================================================================
# HTML equation for plotly
# =============================================================================
.make_plotly_eqn_html <- function(distr, params) {
  if (is.null(params) || length(params) == 0L)
    return(sprintf("[%s distribution]", distr))

  p <- params
  g <- function(nm, d = 4) {
    v <- tryCatch(round(as.numeric(p[nm]), d), error = function(e) NA_real_)
    if (length(v) == 0L || is.na(v)) NA_real_ else v
  }

  tryCatch(switch(distr,

    norm = {
      mu <- g("mean"); sg <- g("sd")
      sprintf(
        "f(y) = 1 / (%.4f \u00b7 \u221a2\u03c0) \u00b7 e<sup>\u2212(y\u2212%.4f)\u00b2/(2\u00b7%.4f\u00b2)</sup>",
        sg, mu, sg)
    },

    lnorm = {
      ml <- g("meanlog"); sl <- g("sdlog")
      sprintf(
        "f(y) = 1 / (y \u00b7 %.4f \u00b7 \u221a2\u03c0) \u00b7 e<sup>\u2212(ln y \u2212 %.4f)\u00b2/(2\u00b7%.4f\u00b2)</sup>",
        sl, ml, sl)
    },

    gamma = {
      sh <- g("shape"); rt <- g("rate"); sh1 <- round(sh - 1, 4)
      sprintf(
        "f(y) = [%.4f<sup>%.4f</sup> / \u0393(%.4f)] \u00b7 y<sup>%.4f</sup> \u00b7 e<sup>\u2212%.4f\u00b7y</sup>",
        rt, sh, sh, sh1, rt)
    },

    weibull = {
      sh <- g("shape"); sc <- g("scale"); sh1 <- round(sh - 1, 4)
      sprintf(
        "f(y) = (%.4f/%.4f) \u00b7 (y/%.4f)<sup>%.4f</sup> \u00b7 exp(\u2212(y/%.4f)<sup>%.4f</sup>)",
        sh, sc, sc, sh1, sc, sh)
    },

    exp = {
      rt <- g("rate")
      sprintf("f(y) = %.4f \u00b7 e<sup>\u2212%.4f\u00b7y</sup>", rt, rt)
    },

    cauchy = {
      loc <- g("location"); sc <- g("scale")
      sprintf(
        "f(y) = 1 / (\u03c0 \u00b7 %.4f \u00b7 [1 + ((y \u2212 %.4f)/%.4f)\u00b2])",
        sc, loc, sc)
    },

    unif = {
      mn <- g("min"); mx <- g("max")
      sprintf("f(y) = 1 / (%.4f \u2212 %.4f)    for %.4f \u2264 y \u2264 %.4f",
              mx, mn, mn, mx)
    },

    sprintf("[%s distribution]", distr)

  ), error = function(e) sprintf("[%s distribution]", distr))
}


# =============================================================================
# 3D ribbon surface plot  (requires plotly)
# =============================================================================
.plot_3d <- function(x, pop_data, H, n_pts, alpha, palette, main, ...) {
  .require_plotly()

  distr  <- x$distr; maxval <- x$maxval; OSB <- x$OSB
  cols   <- rep_len(palette, H)
  x_lo   <- maxval * x$initval
  x_hi   <- max(maxval * x$finval, max(OSB))
  full_B <- c(x_lo, OSB[OSB > x_lo & OSB < x_hi], x_hi)

  x_seq <- seq(x_lo, x_hi, length.out = n_pts)
  y_seq <- seq(0, 1, length.out = 30L)
  dens  <- .eval_density(x_seq, x, use_distr = is.null(pop_data))
  dens[!is.finite(dens)] <- 0

  z_mat <- matrix(rep(dens, times = length(y_seq)),
                  nrow = n_pts, ncol = length(y_seq))

  fig <- plotly::plot_ly() |>
    plotly::add_surface(
      x = x_seq, y = y_seq, z = t(z_mat),
      colorscale = list(
        c(0,    "#D4EBF8"),
        c(0.33, "#52B788"),
        c(0.67, "#1B3A5C"),
        c(1,    "#E9A826")),
      showscale = TRUE,
      colorbar  = list(title = "f(y)", len = 0.45, thickness = 14,
                       x = 0.92, tickfont = list(size = 10)),
      opacity = 0.88, name = "Density surface")

  for (h in seq_len(H)) {
    lo_h <- full_B[h]; hi_h <- full_B[h + 1L]
    idx  <- x_seq >= lo_h & x_seq <= hi_h
    if (sum(idx) < 2L) next
    xs <- c(x_seq[idx], rev(x_seq[idx]))
    zs <- c(dens[idx],  rep(0, sum(idx)))
    fig <- fig |>
      plotly::add_trace(
        x = xs, y = rep(0, length(xs)), z = zs,
        type = "scatter3d", mode = "lines",
        line = list(color = cols[h], width = 4),
        name = sprintf("Stratum %d  Wh=%.3f  nh=%d", h, x$Wh[h], x$nh[h]),
        showlegend = TRUE)
  }
  for (osb_val in OSB) {
    fig <- fig |>
      plotly::add_trace(
        x = c(osb_val, osb_val), y = c(0, 1), z = c(0, 0),
        type = "scatter3d", mode = "lines",
        line = list(color = "#C0392B", width = 5, dash = "dash"),
        name = sprintf("OSB = %.2f", osb_val), showlegend = TRUE)
  }

  # -- Jittered data points (up to 600 sampled) -- only when raw data available
  if (!is.null(pop_data) && length(pop_data) >= 2L) {
    set.seed(42L)
    n_jit   <- min(length(pop_data), 600L)
    idx_jit <- sample.int(length(pop_data), n_jit)
    x_jit   <- pop_data[idx_jit]
    y_jit   <- runif(n_jit, 0, 0.06)                       # shallow depth jitter
    z_jit   <- .eval_density(x_jit, x, use_distr = FALSE)
    z_jit[!is.finite(z_jit)] <- 0
    fig <- fig |>
      plotly::add_trace(
        x = x_jit, y = y_jit, z = z_jit,
        type = "scatter3d", mode = "markers",
        marker = list(size = 2.8, color = "#E9A826",
                      opacity = 0.70,
                      line    = list(width = 0)),
        name      = "Data (jittered)",
        showlegend = TRUE)
  }

  ttl <- main %||% sprintf(
    "3D Density Ribbon  --  stratifyR 2.0<br><sup>H=%d, n=%d, dist=%s</sup>",
    H, x$n, distr)
  fig |> plotly::layout(
    title  = list(text = ttl, font = list(size = 14)),
    scene  = list(
      xaxis  = list(title = "Y (variable)",
                    gridcolor = "#DDEEFF",
                    backgroundcolor = "rgba(240,246,255,0.7)"),
      yaxis  = list(title = "Depth", showticklabels = FALSE,
                    gridcolor = "#DDEEFF",
                    backgroundcolor = "rgba(240,246,255,0.7)"),
      zaxis  = list(title = "Density  f(y)",
                    gridcolor = "#DDEEFF",
                    backgroundcolor = "rgba(240,246,255,0.7)"),
      camera = list(eye = list(x = -1.4, y = -2.0, z = 1.2))),
    legend = list(orientation = "v", x = 1.01, y = 0.9),
    margin = list(t = 60, b = 20, l = 0, r = 120))
}


# =============================================================================
# Interactive slider explorer  (requires plotly)
#
# Layout:
#   TOP BOX  : Distribution name | Greek parameters | PDF equation
#   BELOW    : Optimal OSB values
#   IN PLOT  : OSB labels beside each dashed line
#   RIGHT    : Legend (to avoid slider overlap)
#   BOTTOM   : Slider (starts at optimal OSB)
# =============================================================================
.plot_interactive <- function(x, pop_data, H, n_pts, alpha, palette, main, ...) {
  .require_plotly()

  distr   <- x$distr; maxval <- x$maxval; OSB <- x$OSB
  n_total <- x$n;     N      <- x$N
  cols    <- rep_len(palette, H)

  x_lo   <- maxval * x$initval
  x_hi   <- max(maxval * x$finval, max(OSB))
  full_B <- c(x_lo, OSB[OSB > x_lo & OSB < x_hi], x_hi)

  x_seq  <- seq(x_lo, x_hi, length.out = n_pts)
  dens   <- .eval_density(x_seq, x, use_distr = is.null(pop_data))
  dens[!is.finite(dens)] <- 0

  # Pre-sort raw data for slider objective (same computation as COBYLA optimizer)
  data_sorted_int <- if (!is.null(x$data_internal)) sort(x$data_internal) else NULL

  n_sliders <- H - 1L
  n_steps   <- 31L
  steps_list <- vector("list", n_sliders)
  b_grids    <- vector("list", n_sliders)

  for (bi in seq_len(n_sliders)) {
    b_lo   <- full_B[bi]      + 0.01 * (x_hi - x_lo)
    b_hi   <- full_B[bi + 2L] - 0.01 * (x_hi - x_lo)
    b_grid <- seq(b_lo, b_hi, length.out = n_steps)
    b_grids[[bi]] <- b_grid
    steps_list[[bi]] <- vector("list", n_steps)

    # internal_OSB: only the H-1 internal boundaries (excludes domain endpoints)
    internal_OSB <- full_B[seq(2L, H)]

    for (si in seq_len(n_steps)) {
      b_test  <- sort(replace(internal_OSB, bi, b_grid[si]))
      # Use raw-data objective (matches COBYLA) when data is available
      obj_val <- if (!is.null(data_sorted_int)) {
        .quick_neyman_data(b_test, data_sorted_int, x_lo, x_hi, N)
      } else {
        .quick_neyman_obj(b_test, x_seq, dens, x_lo, x_hi, n_total, N)
      }

      # annotation index 2 = dynamic_annot (top_box=0, ibe_annot=1, dynamic_annot=2)
      steps_list[[bi]][[si]] <- list(
        label  = sprintf("%.2f", b_grid[si]),
        method = "update",
        args   = list(
          list(),   # no trace restyle
          list(`annotations[2].text` = sprintf(
            "b<sub>%d</sub> = %.4f<br>\u03a3W<sub>h</sub>S<sub>h</sub> = %.5f<br>V(n) = %.6f",
            bi, b_grid[si], obj_val, obj_val^2 / n_total))
        )
      )
    }
  }

  # --- base figure -----------------------------------------------------------
  fig <- plotly::plot_ly()

  if (!is.null(pop_data) && length(pop_data) >= 10L) {
    hh <- hist(pop_data, breaks = "Sturges", plot = FALSE)
    fig <- fig |>
      plotly::add_bars(
        x = hh$mids, y = hh$density, name = "Histogram",
        marker = list(color = "rgba(180,180,210,0.45)",
                      line  = list(width = 0.5)),
        width = diff(hh$breaks)[1])
  }

  fig <- fig |>
    plotly::add_trace(
      x = x_seq, y = dens, type = "scatter", mode = "lines",
      line = list(color = "#1a1a2e", width = 2.5),
      name = sprintf("Fitted density (%s)", distr))

  for (h in seq_len(H)) {
    lo_h <- full_B[h]; hi_h <- full_B[h + 1L]
    idx  <- x_seq >= lo_h & x_seq <= hi_h
    if (sum(idx) < 2L) next
    fig <- fig |>
      plotly::add_trace(
        x = c(x_seq[idx], rev(x_seq[idx])),
        y = c(dens[idx], rep(0, sum(idx))),
        type = "scatter", mode = "lines", fill = "toself",
        fillcolor = grDevices::adjustcolor(cols[h], alpha.f = alpha),
        line = list(color = "transparent"),
        name = sprintf("Stratum %d: Wh=%.3f, Sh=%.3f, nh=%d",
                       h, x$Wh[h], round(sqrt(max(x$Vh[h],0)),3), x$nh[h]))
  }

  for (i in seq_along(OSB)) {
    fig <- fig |>
      plotly::add_trace(
        x = rep(OSB[i], 2), y = c(0, max(dens, na.rm=TRUE) * 1.05),
        type = "scatter", mode = "lines",
        line = list(color = "#C0392B", width = 2, dash = "dash"),
        showlegend = FALSE, hoverinfo = "none")
  }

  # --- sliders  (positioned in right panel) ----------------------------------
  sub_digits <- c("\u2081","\u2082","\u2083","\u2084",
                  "\u2085","\u2086","\u2087","\u2088","\u2089")
  slider_specs <- lapply(seq_len(n_sliders), function(bi) {
    opt_step <- which.min(abs(b_grids[[bi]] - OSB[bi])) - 1L
    # Stagger each slider 0.15 paper-units below the previous so they never overlap
    y_pos <- max(0.06, 0.56 - (bi - 1L) * 0.15)
    list(
      active = opt_step,
      currentvalue = list(
        prefix = paste0("b", sub_digits[min(bi, 9L)], " = "),
        font   = list(size = 12, color = "#C0392B")),
      x         = 0.66,
      y         = y_pos,
      len       = 0.31,
      xanchor   = "left",
      yanchor   = "top",
      pad       = list(t = 35, b = 5),
      steps     = steps_list[[bi]])
  })

  # --- build all annotations -------------------------------------------------
  params_for_eqn <- tryCatch({
    fit <- x$fit
    if (!is.null(fit$estimate))          fit$estimate
    else if (!is.null(fit$fit$estimate)) fit$fit$estimate
    else NULL
  }, error = function(e) NULL)

  eqn_html   <- .make_plotly_eqn_html(distr, params_for_eqn)
  greek_html <- .greek_param_html(distr, params_for_eqn)
  dist_name  <- .dist_display_name(distr)

  rp <- 0.825   # centre of right panel  (plot domain 0--0.63)

  # [0] Top box -- distribution name + params + equation, FULL WIDTH at top
  top_box <- list(
    x = 0.5, y = 1.18,
    xref = "paper", yref = "paper",
    xanchor = "center", yanchor = "bottom",
    text = paste0(
      "<b>", dist_name, "</b>  |  ",
      .greek_param_html(distr, params_for_eqn, sep = "  ,  "),
      "<br>", eqn_html),
    showarrow   = FALSE,
    font        = list(size = 13, color = "#1a1a2e"),
    bgcolor     = "#EEF2FF",
    bordercolor = "#7090CC",
    borderwidth = 1.5,
    borderpad   = 9
  )

  # [1] IBE static header box -- right panel
  ibe_annot <- list(
    x = rp, y = 0.52,
    xref = "paper", yref = "paper",
    xanchor = "center", yanchor = "top",
    text = sprintf(
      "<b>Interactive Boundary Explorer</b><br>%d Strata  |  n = %d",
      H, n_total),
    showarrow   = FALSE,
    font        = list(size = 11.5, color = "#222"),
    bgcolor     = "#F0F4FF",
    bordercolor = "#9090BB",
    borderwidth = 1,
    borderpad   = 8
  )

  # [2] Dynamic values -- right panel, updated by slider steps (index 2)
  init_dyn <- sprintf(
    "b<sub>opt</sub> = %s<br>\u03a3W<sub>h</sub>S<sub>h</sub> = %.5f<br>V* = %.6f",
    paste(sprintf("%.4f", OSB), collapse = ", "),
    x$WhShTot, x$WhShTot^2 / n_total)
  dynamic_annot <- list(
    x = rp, y = 0.75,
    xref = "paper", yref = "paper",
    xanchor = "center", yanchor = "top",
    text = init_dyn,
    showarrow   = FALSE,
    font        = list(size = 11.5, color = "#C0392B"),
    bgcolor     = "rgba(255,240,240,0.9)",
    bordercolor = "#C0392B",
    borderwidth = 1,
    borderpad   = 7
  )

  # OSB labels beside each dashed line (plot area)
  x_rng <- x_hi - x_lo
  d_top <- max(dens, na.rm = TRUE)
  osb_lbl <- lapply(seq_along(OSB), function(i) {
    space_right <- (x_hi - OSB[i]) / x_rng
    xanchor     <- if (space_right > 0.12) "left" else "right"
    xshift      <- if (space_right > 0.12) 7 else -7
    list(
      x = OSB[i], y = d_top * 0.97,
      xref = "x", yref = "y",
      xanchor = xanchor, yanchor = "top",
      xshift  = xshift,
      text = sprintf("<b>b<sub>%d</sub> = %.3f</b>", i, OSB[i]),
      showarrow = FALSE,
      font = list(size = 11, color = "#C0392B"))
  })

  # order matters: top_box=0, ibe_annot=1, dynamic_annot=2, then osb labels
  all_annots <- c(list(top_box, ibe_annot, dynamic_annot), osb_lbl)

  fig |> plotly::layout(
    title     = list(text = "", font = list(size = 1)),
    xaxis     = list(domain = c(0, 0.63), title = "Y  (variable value)"),
    yaxis     = list(title = "Density  f(y)"),
    sliders   = slider_specs,
    legend    = list(orientation = "v", x = 0.66, y = 0.98,
                     xanchor = "left", yanchor = "top",
                     bgcolor     = "rgba(255,255,255,0.85)",
                     bordercolor = "#ccc", borderwidth = 1),
    margin    = list(t = 140, b = max(80, 80 + (n_sliders - 1L) * 55), r = 20, l = 60),
    hovermode = "x unified",
    annotations = all_annots)
}


# =============================================================================
# Utility helpers
# =============================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

.require_plotly <- function() {
  if (!requireNamespace("plotly", quietly = TRUE))
    stop("Package 'plotly' is required.\nInstall with: install.packages('plotly')",
         call. = FALSE)
}

.eval_density <- function(x_vals, strata_obj, use_distr = FALSE) {
  distr <- strata_obj$distr
  fit   <- strata_obj$fit
  params <- tryCatch({
    if (!is.null(fit$estimate))          fit$estimate
    else if (!is.null(fit$fit$estimate)) fit$fit$estimate
    else if (is.list(fit) && !is.null(fit[["estimate"]])) fit[["estimate"]]
    else NULL
  }, error = function(e) NULL)

  if (is.null(params))
    return(rep(1 / (max(x_vals) - min(x_vals)), length(x_vals)))

  p <- params
  tryCatch(switch(distr,
    norm    = stats::dnorm(x_vals,   mean     = p["mean"],    sd       = p["sd"]),
    lnorm   = stats::dlnorm(x_vals,  meanlog  = p["meanlog"], sdlog    = p["sdlog"]),
    gamma   = stats::dgamma(x_vals,  shape    = p["shape"],   rate     = p["rate"]),
    weibull = stats::dweibull(x_vals, shape   = p["shape"],   scale    = p["scale"]),
    exp     = stats::dexp(x_vals,    rate     = p["rate"]),
    cauchy  = stats::dcauchy(x_vals, location = p["location"], scale   = p["scale"]),
    unif    = stats::dunif(x_vals,   min      = p["min"],     max      = p["max"]),
    {
      if (!is.null(strata_obj$data_internal)) {
        d <- density(strata_obj$data_internal, n = length(x_vals),
                     from = min(x_vals), to = max(x_vals))
        stats::approx(d$x, d$y, xout = x_vals, rule = 2)$y
      } else {
        rep(1 / (max(x_vals) - min(x_vals)), length(x_vals))
      }
    }
  ), error = function(e)
    rep(1 / (max(x_vals) - min(x_vals)), length(x_vals)))
}

.quick_neyman_obj <- function(b_test, x_seq, dens, x_lo, x_hi, n_total, N) {
  H      <- length(b_test) + 1L
  full_B <- c(x_lo, sort(b_test), x_hi)
  total_area <- sum(diff(x_seq) * (head(dens,-1) + tail(dens,-1))) / 2
  Wh_v <- numeric(H); Sh_v <- numeric(H)
  for (h in seq_len(H)) {
    lo_h <- full_B[h]; hi_h <- full_B[h+1L]
    idx  <- x_seq >= lo_h & x_seq < hi_h
    if (sum(idx) < 2L) next
    xs <- x_seq[idx]; fs <- dens[idx]; dxs <- diff(xs)
    Wh_v[h] <- sum(dxs*(head(fs,-1)+tail(fs,-1)))/2/max(total_area,1e-12)
    if (Wh_v[h] < 1e-12) next
    mu_h  <- sum(dxs*(head(xs*fs,-1)+tail(xs*fs,-1)))/2/Wh_v[h]/max(total_area,1e-12)
    mu2_h <- sum(dxs*(head(xs^2*fs,-1)+tail(xs^2*fs,-1)))/2/Wh_v[h]/max(total_area,1e-12)
    Sh_v[h] <- sqrt(max(mu2_h - mu_h^2, 0))
  }
  sum(Wh_v * Sh_v)
}

# Data-based objective - mirrors .cobyla_obj_data() exactly.
# Uses the sorted raw population data so the slider sumWhSh values are
# identical to those produced by the COBYLA optimizer.
.quick_neyman_data <- function(b_test, data_sorted, x_lo, x_hi, N) {
  H      <- length(b_test) + 1L
  full_B <- c(x_lo, sort(b_test), x_hi)
  obj    <- 0
  for (h in seq_len(H)) {
    lo    <- full_B[h]
    hi    <- full_B[h + 1L]
    slice <- if (h == 1L) data_sorted[data_sorted >= lo & data_sorted <= hi]
              else         data_sorted[data_sorted >  lo & data_sorted <= hi]
    Nh <- length(slice)
    if (Nh < 2L) next
    Wh  <- Nh / N
    Sh  <- sqrt(stats::var(slice))
    obj <- obj + Wh * Sh
  }
  obj
}
