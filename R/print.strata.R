# =============================================================================
# print.strata.R  --  stratifyR 2.0
# =============================================================================

#' Print a stratification
#'
#' Prints a compact console summary of the stratification results. For the full
#' per-stratum table use \code{summary(x)}.
#'
#'
#' @param x A \code{"stratifyR"} object.
#' @param ... Currently unused.
#' @return Invisibly returns \code{x}.
#' @rdname print.strata
#' @export
print.stratifyR <- function(x, ...) {
  H <- nrow(x$h)

  cat("-- stratifyR 2.0 -- Optimal Stratification Results ------------------\n")
  cat(sprintf("  Distribution : %s\n",    x$distr))
  cat(sprintf("  Strata (L)   : %d\n",    H))
  cat(sprintf("  Sample size  : n = %d",  x$n))
  if (!is.null(x$N) && !is.na(x$N)) cat(sprintf(",  N = %d", x$N))
  cat("\n")
  cat(sprintf("  Method       : %s\n",    x$method %||% "dp"))
  cat(sprintf("  Converged    : %s\n",    if (isTRUE(x$converged)) "yes" else "no / dp"))

  gap <- x$optimality_gap
  if (!is.null(gap) && is.finite(gap))
    cat(sprintf("  Optimality gap (C-S): %.4f%%\n", gap * 100))

  cat("\n")
  cat(sprintf("  OSB  : %s\n",
              paste(sprintf("%.4g", x$OSB), collapse = " | ")))
  cat(sprintf("  nh   : %s\n",
              paste(x$nh, collapse = " | ")))
  cat(sprintf("  Wh   : %s\n",
              paste(sprintf("%.4f", x$Wh), collapse = " | ")))
  cat(sprintf("  WhSh : %s\n",
              paste(sprintf("%.5f", x$WhSh), collapse = " | ")))
  cat(sprintf("  V*   : %.6f\n",   (x$WhShTot)^2 / x$n))
  cat("----------------------------------------------------------------------\n")

  invisible(x)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
