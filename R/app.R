# =============================================================================
# app.R  -  stratifyR 2.0
#
# Shiny app launcher. Defines stratifyRApp(), the user-facing function that
# opens the interactive web interface. The application itself lives in
# inst/app/app.R and is located at run time with system.file().
# =============================================================================

#' Launch the stratifyR 2.0 Interactive Shiny Application
#'
#' Opens the \pkg{stratifyR} web interface in the default browser. The
#' application gives no-code access to the package: users upload their own data
#' (CSV or Excel) or select a built-in dataset, compute optimum stratum
#' boundaries and sample sizes with the \code{"dp"}, \code{"cobyla"},
#' \code{"global"} or \code{"exact"} solvers, compare design efficiencies,
#' explore boundaries interactively, and download the results.
#'
#' @details
#' The application needs \pkg{shiny}, \pkg{bslib} and \pkg{DT} to start; if any
#' of these is missing, \code{stratifyRApp()} stops with a short installation
#' hint rather than a cryptic error. The optional packages \pkg{plotly},
#' \pkg{readxl}, \pkg{ggplot2} and \pkg{stratification} add features
#' (interactive 2D/3D and slider plots, Excel upload, and the
#' Lavallee-Hidiroglou comparison). The app opens without them, and on launch it
#' reports any that are not installed so nothing fails silently. To install
#' everything the app can use:
#' \preformatted{install.packages(c("shiny", "bslib", "DT",
#'                    "plotly", "readxl", "ggplot2", "stratification"))}
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}, for
#'   example \code{port} or \code{launch.browser}.
#'
#' @return Called for its side effect (launches a Shiny app); returns
#'   \code{NULL} invisibly.
#'
#' @examples
#' \dontrun{
#' stratifyRApp()
#' }
#'
#' @export
stratifyRApp <- function(...) {
  ## Packages the application cannot start without.
  need <- c("shiny", "bslib", "DT")
  miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss) > 0L)
    stop("The stratifyR app needs the package(s): ",
         paste(miss, collapse = ", "), ".\n",
         "Install everything the app uses with:\n",
         '  install.packages(c("shiny", "bslib", "DT", "plotly", ',
         '"readxl", "ggplot2", "stratification"))',
         call. = FALSE)

  ## Optional packages: the app opens without them, but some panels are limited.
  opt      <- c("plotly", "readxl", "ggplot2", "stratification")
  opt_miss <- opt[!vapply(opt, requireNamespace, logical(1), quietly = TRUE)]
  if (length(opt_miss) > 0L)
    message("stratifyR: the app will open, but these optional package(s) are ",
            "not installed, so some features are limited: ",
            paste(opt_miss, collapse = ", "), ".\n",
            "For the full app, install.packages(c(",
            paste(sprintf('"%s"', opt_miss), collapse = ", "), "))")

  app_dir <- system.file("app", package = "stratifyR")
  if (!nzchar(app_dir) || !dir.exists(app_dir))
    stop("Could not find the stratifyR app directory. ",
         "Try reinstalling the package.", call. = FALSE)

  shiny::runApp(app_dir, ...)
}
