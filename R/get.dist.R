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
      res <- try(fitter(), silent = TRUE)
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
   
   # ---- Pareto (actuar::pareto), positive-only ------------------------------
   if (pos_only && requireNamespace("actuar", quietly = TRUE)) {
      fits$pareto <- try_fit(function() {
         fitdistrplus::fitdist(
            x, distr = "pareto", method = "mle",
            start = list(shape = 1, scale = stats::quantile(x, 0.25)),
            lower = c(1e-8, 1e-8)
         )
      })
   }
   
   # ---- Triangular (mc2d::triang) -- fix min/max; estimate only mode --------
   if (requireNamespace("mc2d", quietly = TRUE)) {
      minx <- min(x); maxx <- max(x)
      if (is.finite(minx) && is.finite(maxx) && minx < maxx) {
         # empirical mode proxy (histogram bin with max count)
         br <- pretty(x, n = 20)
         hh <- hist(x, breaks = br, plot = FALSE)
         m0 <- hh$mids[which.max(hh$counts)]
         # keep mode strictly inside (minx, maxx)
         eps <- (maxx - minx) * 1e-6 + 1e-8
         m0  <- min(max(m0, minx + eps), maxx - eps)
         
         fits$triang <- try_fit(function() {
            fitdistrplus::fitdist(
               x, distr = "triang", method = "mle",
               start   = list(mode = m0),
               fix.arg = list(min = minx, max = maxx)
            )
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
   fits$cauchy <- try_fit(function() {
      fitdistrplus::fitdist(x, "cauchy", method = "mle")
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