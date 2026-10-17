#' Determine best-fit distribution
#'Identify the best-fit distribution for a univariate numeric vector

#' @param data Input dataset.
#' @param my_env Environment object.
#' @return ...
#'
#' Fits several candidate distributions via MLE and selects the model with the
#' lowest (finite) AIC. For strictly positive data, tail-sensitive families are
#' also tried. If a triangular fit narrowly wins (Delta AIC <= 10 over a tail model,
#' prefer the tail model (helps avoid spurious triangular wins on mildly skewed data).
#'
#' Returns a list with:
#'  - distr:   best model name (e.g., "gamma", "weibull", "triangle", ...)
#'  - params:  named parameter vector for the best model
#'  - aic:     named numeric vector of AICs for all attempted models (NA if failed)
#'  - fits_ok: named logical vector (TRUE where fit succeeded)
#'  - messages:named list of diagnostic messages per model (errors/warnings)
#'
#' @import fitdistrplus
#' @importFrom stats quantile var sd
get.dist <- function(data, my_env) {
   # ---- preflight -----------------------------------------------------------
   stopifnot(is.numeric(data))
   x <- as.numeric(data)
   x <- x[is.finite(x)]
   if (length(x) < 5L) stop("get.dist(): need >= 5 finite observations")
   
   pos_only <- all(x > 0)  # some families only make sense for x > 0
   
   # Helper: run a fit safely; capture AIC/estimates or a message
   try_fit <- function(fitter) {
      out <- list(ok = FALSE, aic = NA_real_, estimate = NULL, msg = NULL)
      # fitdistrplus prints optimiser errors and warns about default
      # parameters when a family does not fit; those families are simply
      # skipped here, so keep their messages out of the user's console.
      res <- try({
         utils::capture.output(fit_res <- suppressWarnings(fitter()))
         fit_res
      }, silent = TRUE)
      if (inherits(res, "try-error")) {
         out$msg <- as.character(res)
         return(out)
      }
      if (!is.null(res$aic) && is.finite(res$aic)) {
         out$ok <- TRUE
         out$aic <- as.numeric(res$aic)
         out$estimate <- res$estimate
      } else {
         out$msg <- "AIC not finite"
      }
      out
   }
   
   fits <- list()
   
   # ---- Pareto type II (Lomax), positive-only ---------------------------------
   # v2.0-3: fitted directly by maximum likelihood. fitdistrplus::fitdist()
   # looks for dpareto() on the user's search path, so inside the package the
   # Pareto fit failed unless the user had attached actuar themselves.
   if (pos_only) {
      fits$pareto <- try_fit(function() {
         n  <- length(x)
         # Written as log f = log(a/s) - (a + 1) log(1 + x/s). The textbook
         # form n a log(s) - (a + 1) sum(log(x + s)) subtracts two huge,
         # nearly equal numbers when a and s are large, and the rounding
         # error could make a poor Pareto fit look like the best one
         # (seen on Apple silicon for Gamma, Weibull, uniform and
         # triangular data).
         nll <- function(p) {
            if (any(abs(p) > 50)) return(1e300)
            a <- exp(p[1]); s <- exp(p[2])
            v <- -(n * (p[1] - p[2]) - (a + 1) * sum(log1p(x / s)))
            if (is.finite(v)) v else 1e300
         }
         op <- stats::optim(c(log(2), log(stats::median(x))), nll,
                            method = "Nelder-Mead",
                            control = list(maxit = 2000, reltol = 1e-12))
         op <- stats::optim(op$par, nll, method = "BFGS",
                            control = list(maxit = 500, reltol = 1e-14))
         if (!is.finite(op$value)) stop("Pareto fit did not converge")
         list(aic = 2 * 2 + 2 * op$value,
              estimate = c(shape = exp(op$par[1]), scale = exp(op$par[2])))
      })
   }
   
   # ---- Triangular -----------------------------------------------------------
   # v2.0-3: fitted directly. min and max are set just outside the sample
   # range, by one average spacing (range / (n - 1)) on each side, because the
   # triangular density is zero at its end points and the likelihood would be
   # zero if they equalled the smallest and largest values. The maximum
   # likelihood mode then lies at one of the data values, so the likelihood is
   # evaluated at each distinct value (no optimiser, no search-path lookup).
   {
      minx <- min(x); maxx <- max(x); n <- length(x)
      if (is.finite(minx) && is.finite(maxx) && minx < maxx && n >= 3L) {
         fits$triang <- try_fit(function() {
            ext <- (maxx - minx) / (n - 1)
            a <- minx - ext; b <- maxx + ext
            cand <- sort(unique(x))
            if (length(cand) > 2000L)
               cand <- unique(stats::quantile(x, seq(0, 1, length.out = 2000L), names = FALSE))
            ll <- vapply(cand, function(m) {
               f <- ifelse(x <= m, 2 * (x - a) / ((b - a) * (m - a)),
                                   2 * (b - x) / ((b - a) * (b - m)))
               sum(log(f))
            }, 0)
            k <- which.max(ll)
            list(aic = 2 * 3 - 2 * ll[k],
                 estimate = c(min = a, max = b, mode = cand[k]))
         })
      }
   }
   
   # ---- Weibull (positive-only) ---------------------------------------------
   if (pos_only) {
      fits$weibull <- try_fit(function() {
         fitdistrplus::fitdist(x, "weibull", method = "mle", lower = c(1e-8, 1e-8))
      })
   }
   
   # ---- Gamma (positive-only) -----------------------------------------------
   if (pos_only) {
      fits$gamma <- try_fit(function() {
         m <- mean(x); v <- stats::var(x)
         rate0  <- if (v > 0) m / v else 1
         shape0 <- if (v > 0) m * rate0 else 1
         fitdistrplus::fitdist(
            x, "gamma", method = "mle",
            start = list(shape = shape0, rate = rate0),
            lower = c(1e-8, 1e-8)
         )
      })
   }
   
   # ---- Exponential (positive-only) -----------------------------------------
   if (pos_only) {
      fits$exp <- try_fit(function() {
         fitdistrplus::fitdist(x, "exp", method = "mle", start = list(rate = 1 / mean(x)))
      })
   }
   
   # ---- Uniform --------------------------------------------------------------
   fits$unif <- try_fit(function() {
      fitdistrplus::fitdist(
         x, "unif", method = "mle",
         start = list(min = min(x), max = max(x))
      )
   })
   
   # ---- Normal ---------------------------------------------------------------
   fits$norm <- try_fit(function() {
      fitdistrplus::fitdist(x, "norm", method = "mle")
   })
   
   # ---- Lognormal (positive-only) -------------------------------------------
   if (pos_only) {
      fits$lnorm <- try_fit(function() {
         fitdistrplus::fitdist(x, "lnorm", method = "mle")
      })
   }
   
   # ---- Cauchy ---------------------------------------------------------------
   # v2.0-3: start at the median and half the interquartile range, and keep
   # the scale positive; the unconstrained default often failed to converge.
   fits$cauchy <- try_fit(function() {
      fitdistrplus::fitdist(
         x, "cauchy", method = "mle",
         start = list(location = stats::median(x),
                      scale    = max(stats::IQR(x) / 2, 1e-8)),
         lower = c(-Inf, 1e-12)
      )
   })
   
   # ---- collect & choose -----------------------------------------------------
   aics <- vapply(fits, function(z) if (isTRUE(z$ok)) z$aic else NA_real_, 0.0)
   ok   <- vapply(fits, function(z) isTRUE(z$ok), FALSE)
   msgs <- lapply(fits, function(z) z$msg)
   
   finite_idx <- is.finite(aics)
   if (any(finite_idx)) {
      aics_f <- aics[finite_idx]
      
      # Best overall among finite AICs
      best_name <- names(aics_f)[which.min(aics_f)]
      best_aic  <- aics[[best_name]]
      
      # Best tail model among {gamma, weibull, lnorm, exp} when available
      tail_models <- intersect(names(aics_f), c("gamma", "weibull", "lnorm", "exp"))
      if (length(tail_models) > 0) {
         tail_aics_f   <- aics[tail_models]
         best_tail     <- tail_models[which.min(tail_aics_f)]
         best_tail_aic <- aics[[best_tail]]
      } else {
         best_tail     <- NA_character_
         best_tail_aic <- Inf
      }
      
      # If triangle "wins" but not decisively, prefer the best tail model
      if (identical(best_name, "triang") && is.finite(best_tail_aic)) {
         if ((best_tail_aic - best_aic) <= 10) {
            best_name <- best_tail
            best_aic  <- best_tail_aic
         }
      }
      
      best_fit <- fits[[best_name]]
      params   <- best_fit$estimate
      
      # --- SAFETY for triangular output shape/length/naming -------------------
      if (best_name == "triang") {
         # If only 'mode' was estimated (min/max were fixed), rebuild a full
         # (min, max, mode) vector using sample min/max (the fixed values).
         if (length(params) == 1L) {
            params <- c(min = min(x), max = max(x), mode = as.numeric(params))
         } else {
            # If 3 params present but names are odd, normalize them.
            nn <- names(params)
            if (length(params) == 3L && !all(c("min","max","mode") %in% nn)) {
               names(params) <- c("min","max","mode")
            }
         }
      }
      
      result <- list(
         distr    = if (best_name == "triang") "triangle" else best_name,
         params   = params,
         aic      = aics,
         fits_ok  = ok,
         messages = msgs
      )
   } else {
      # Fallback: normal with simple estimates
      result <- list(
         distr    = "norm",
         params   = c(mean = mean(x), sd = stats::sd(x)),
         aic      = aics,
         fits_ok  = ok,
         messages = msgs
      )
   }
   
   return(result)
}
#####################################################