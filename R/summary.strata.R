# =============================================================================
# summary.strata.R  -  stratifyR 2.1
#
# summary() builds a "summary.strata" object and prints nothing. Its print()
# method shows the table in the console (coloured when the console supports
# it), knit_print() shows a styled table in R Markdown, and as.data.frame()
# returns the table. Before 2.1-0, summary() printed the table itself and
# returned a data frame.
# =============================================================================

#' Summarise a stratification
#'
#' Builds a summary of a \code{"strata"} object: the design settings, the
#' fitted distribution and a table with one row per stratum and a total row.
#' \code{summary()} prints nothing itself; the result is printed by its
#' \code{print()} method (or automatically at the console), and
#' \code{as.data.frame()} returns the table.
#'
#' In the console, the boundary and \eqn{W_h S_h} columns are shown in purple
#' and the total row in bold gold when the console supports colour (through
#' the \pkg{crayon} package; set \code{options(stratifyR.color = FALSE)} to
#' turn this off). In R Markdown, the table is shown as a styled HTML table.
#'
#' @param object An object of class \code{"strata"}, from
#'   \code{\link{strata.data}} or \code{\link{strata.distr}}.
#' @param x An object of class \code{"summary.strata"}.
#' @param row.names,optional Not used; for compatibility with the generic.
#' @param ... Not used.
#' @return \code{summary()} returns an object of class \code{"summary.strata"},
#'   a list with components:
#' \describe{
#'   \item{\code{L}}{Number of strata.}
#'   \item{\code{n}, \code{N}}{Sample size and population size.}
#'   \item{\code{method}}{The solver used.}
#'   \item{\code{distr}, \code{estimate}}{The best-fit distribution and its
#'     parameter estimates.}
#'   \item{\code{range}}{The smallest and largest values of the variable.}
#'   \item{\code{cost}, \code{ch}}{Whether stratum costs were used, and the costs.}
#'   \item{\code{takeall}, \code{fpc}, \code{variance}}{The take-all stratum
#'     size, whether the finite population correction was used, and the
#'     variance of the mean (for \code{fpc = TRUE} or \code{takeall = TRUE}).}
#'   \item{\code{table}}{A data frame with one row per stratum and a total row:
#'     the upper boundary (\code{OSB}), \code{Wh}, \code{Vh} (the stratum
#'     variance \eqn{S_h^2}), \code{WhSh}, \code{nh}, \code{Nh} and \code{fh}.}
#' }
#'   \code{print()} returns \code{x} invisibly, and \code{as.data.frame()}
#'   returns \code{x$table}.
#' @seealso \code{\link{strata.data}}, \code{\link{boundaries}},
#'   \code{\link{allocation}}, \code{\link{objective}}
#' @examples
#' \donttest{
#' data(sugarcane)
#' res <- strata.data(sugarcane$Production, h = 4, n = 500, method = "exact")
#' s <- summary(res)
#' s
#' as.data.frame(s)
#' }
#' @export
summary.strata <- function(object, ...) {
   `%||%` <- function(a, b) if (!is.null(a)) a else b
   h <- nrow(object$h)
   fit <- object$fit
   param_est <- tryCatch({
      if (!is.null(fit$estimate)) fit$estimate else fit$fit$estimate
   }, error = function(e) NULL)

   # OSB: pad or trim to exactly h values so the table rows match.
   osb_vec <- signif(as.numeric(object$OSB), 6)
   if (length(osb_vec) < h) {
      final_val <- round(object$maxval * object$finval, 2)
      osb_vec   <- c(osb_vec, rep(final_val, h - length(osb_vec)))
   } else {
      osb_vec <- osb_vec[seq_len(h)]
   }

   tab2 <- data.frame(
      Strata = object$h$Strata %||% seq_len(h),
      OSB    = osb_vec,
      Wh     = object$Wh,
      Vh     = object$Vh,
      WhSh   = object$WhSh,
      nh     = object$nh,
      Nh     = object$Nh,
      fh     = object$fh,
      check.names = FALSE
   )
   tab3 <- data.frame(
      Strata = "Total",
      OSB    = "",
      Wh     = object$WhTot,
      Vh     = object$VhTot,
      WhSh   = object$WhShTot,
      nh     = object$nhTot,
      Nh     = object$NhTot,
      fh     = object$fhTot,
      check.names = FALSE
   )

   structure(list(
      L        = h,
      n        = object$n,
      N        = object$N,
      method   = object$method %||% "dp",
      distr    = object$distr,
      estimate = param_est,
      range    = c(lower = object$maxval * object$initval,
                   upper = object$maxval * object$finval),
      width    = object$maxval * object$dist,
      cost     = isTRUE(object$cost),
      ch       = object$ch,
      takeall  = object$takeall %||% 0L,
      fpc      = isTRUE(object$fpc),
      variance = object$variance,
      table    = rbind(tab2, tab3)),
      class = "summary.strata")
}

#' @rdname summary.strata
#' @export
as.data.frame.summary.strata <- function(x, row.names = NULL, optional = FALSE, ...) {
   x$table
}

# -----------------------------------------------------------------------------
# Shared formatting helpers
# -----------------------------------------------------------------------------
.sum_hrule <- function(char = "-", width = getOption("width", 80)) {
   paste(rep(char, max(40L, min(120L, width))), collapse = "")
}

.sum_has_crayon <- function() {
   want_color <- isTRUE(getOption("stratifyR.color", TRUE))
   want_color &&
      requireNamespace("crayon", quietly = TRUE) &&
      isTRUE(getOption("crayon.enabled", TRUE)) &&
      !isTRUE(as.logical(Sys.getenv("NO_COLOR", "0"))) &&
      isTRUE(try(crayon::has_color(), silent = TRUE))
}

.sum_purple <- "#5A189A"  # OSB and WhSh columns
.sum_gold   <- "#CC9A00"  # total row (HTML)

.sum_col <- function(x, role = NULL, has_crayon) {
   if (!has_crayon || is.null(role)) return(x)
   purple <- crayon::make_style(.sum_purple)
   switch(
      role,
      title = crayon::bold(crayon::cyan(x)),
      key   = crayon::blue(x),
      val   = crayon::bold(crayon::cyan(x)),
      hdr   = crayon::green(x),
      osb   = crayon::combine_styles(purple, crayon::bold)(x),
      whsh  = crayon::combine_styles(purple, crayon::bold)(x),
      tot   = crayon::combine_styles(crayon::yellow, crayon::bold)(x),
      x
   )
}

.sum_fmt_params <- function(est, digits = 6) {
   if (is.null(est) || length(est) == 0L || all(is.na(est))) return("(no parameter estimates)")
   nm <- names(est); if (is.null(nm)) nm <- paste0("par", seq_along(est))
   key_w <- max(nchar(nm), 4L)
   vals <- vapply(est, function(v) if (is.na(v)) "NA" else formatC(v, digits = digits, format = "g"),
                  FUN.VALUE = character(1))
   paste(sprintf(paste0("  %-", key_w, "s : %s"), nm, vals), collapse = "\n")
}

.sum_format_col <- function(v, name) {
   if (!is.numeric(v)) return(as.character(v))
   digits <- switch(name, OSB = 2L, Wh = 2L, Vh = 2L, WhSh = 3L, fh = 2L, nh = 0L, Nh = 0L, 3L)
   if (digits == 0L) formatC(v, format = "d", big.mark = ",")
   else              formatC(v, digits = digits, format = "f", big.mark = ",")
}

.sum_table_console <- function(df, has_crayon) {
   raw_cols <- lapply(seq_along(df), function(j) .sum_format_col(df[[j]], names(df)[j]))
   names(raw_cols) <- names(df)
   widths <- vapply(seq_along(raw_cols), function(j)
      max(nchar(names(df)[j]), nchar(raw_cols[[j]]), na.rm = TRUE), 0L)
   header_cells <- mapply(function(h, w) sprintf(paste0("%-", w, "s"), h),
                          names(df), widths, SIMPLIFY = FALSE)
   if (has_crayon) header_cells <- lapply(header_cells, .sum_col, role = "hdr",
                                          has_crayon = has_crayon)
   header <- paste(header_cells, collapse = "  ")
   nR <- nrow(df)
   rows <- vapply(seq_len(nR), function(i) {
      cells <- mapply(function(col, w, nm) {
         cell <- sprintf(paste0("%-", w, "s"), col[i])
         if (has_crayon && i < nR) {
            if (nm == "OSB")  cell <- .sum_col(cell, "osb", has_crayon)
            if (nm == "WhSh") cell <- .sum_col(cell, "whsh", has_crayon)
         }
         if (has_crayon && i == nR) cell <- .sum_col(cell, "tot", has_crayon)
         cell
      }, raw_cols, widths, names(df), SIMPLIFY = FALSE)
      paste(cells, collapse = "  ")
   }, FUN.VALUE = character(1))
   c(header, rows)
}

.sum_range_txt <- function(x) {
   sprintf("[%.4f, %.4f]  (d = %.4f)", x$range[["lower"]], x$range[["upper"]], x$width)
}

#' @rdname summary.strata
#' @export
print.summary.strata <- function(x, ...) {
   hc  <- .sum_has_crayon()
   col <- function(v, role) .sum_col(v, role, hc)
   h <- x$L; N <- x$N
   cat(.sum_hrule("="), "\n", sep = "")
   cat(col("Stratification summary", "title"), "\n", sep = "")
   cat(.sum_hrule("-"), "\n", sep = "")
   cat(col("Strata (h): ", "key"), col(h, "val"), "    ",
       col("n: ", "key"),       col(x$n, "val"), "    ",
       col("N: ", "key"),       col(if (is.null(N)) "NA" else as.character(N), "val"),
       "\n", sep = "")
   cat(col("Data range: ", "key"), .sum_range_txt(x), "\n", sep = "")
   if (x$cost && !is.null(x$ch))
      cat(col("Stratum costs Ch: ", "key"), paste0("[", paste(x$ch, collapse = ", "), "]"),
          "\n", sep = "")
   cat(col("Best-fit distribution: ", "key"), x$distr, "\n", sep = "")
   if (isTRUE(x$takeall > 0))
      cat(col("Take-all stratum: ", "key"),
          sprintf("stratum %d (%s units, sampled completely)", h,
                  format(x$takeall, big.mark = ",")), "\n", sep = "")
   if (isTRUE(x$fpc) && !is.null(x$variance))
      cat(col("Variance of the mean (with fpc): ", "key"),
          formatC(x$variance, digits = 6, format = "g"), "\n", sep = "")
   cat(col("Parameter estimate(s):", "key"), "\n", sep = "")
   cat(.sum_fmt_params(x$estimate), "\n", sep = "")
   cat(.sum_hrule("-"), "\n", sep = "")
   cat(paste(.sum_table_console(x$table, hc), collapse = "\n"), "\n", sep = "")
   cat(.sum_hrule("="), "\n", sep = "")
   invisible(x)
}

# R Markdown: a styled HTML table (registered on load when knitr is present).
knit_print.summary.strata <- function(x, ...) {
   is_html <- isTRUE(try(knitr::is_html_output(), silent = TRUE))
   h <- x$L; N <- x$N
   rng_txt <- .sum_range_txt(x)
   pe <- if (!is.null(x$estimate) && length(x$estimate) > 0L && !all(is.na(x$estimate)))
      data.frame(Parameter = names(x$estimate), Estimate = as.numeric(x$estimate),
                 check.names = FALSE) else NULL
   digits <- c(0, 2, 2, 2, 3, 0, 0, 2)
   tab <- x$table

   if (is_html) {
      hdr <- c(
         "<div style='margin-bottom:0.5rem'><span style='color:#0b7285;font-weight:700'>Stratification summary</span></div>",
         paste0(sprintf("<div><span style='color:#0b7285;font-weight:600'>Strata (h):</span> %s &nbsp; ", h),
                sprintf("<span style='color:#0b7285;font-weight:600'>n:</span> %s &nbsp; ", x$n),
                sprintf("<span style='color:#0b7285;font-weight:600'>N:</span> %s</div>",
                        if (is.null(N)) "NA" else N)),
         sprintf("<div><span style='color:#0b7285;font-weight:600'>Data range:</span> %s</div>", rng_txt))
      if (x$cost && !is.null(x$ch))
         hdr <- c(hdr, sprintf("<div><span style='color:#0b7285;font-weight:600'>Stratum costs Ch:</span> [%s]</div>",
                               paste(x$ch, collapse = ", ")))
      hdr <- c(hdr, sprintf("<div><span style='color:#0b7285;font-weight:600'>Best-fit distribution:</span> %s</div>\n",
                            x$distr))
      hdr <- c(hdr, if (!is.null(pe))
         as.character(knitr::kable(pe, format = "html", digits = 6, align = c("l", "r"),
                                   caption = "Parameter estimate(s)"))
         else "<p>(no parameter estimates)</p>")
      if (requireNamespace("kableExtra", quietly = TRUE)) {
         kb <- knitr::kable(tab, caption = "Per-stratum results and totals",
                            align = rep("r", 8), digits = digits, format = "html", escape = TRUE) |>
            kableExtra::kable_styling(full_width = FALSE, bootstrap_options = c("condensed")) |>
            kableExtra::row_spec(0, bold = TRUE, color = "#2b8a3e") |>
            kableExtra::column_spec(2, color = .sum_purple) |>
            kableExtra::column_spec(5, color = .sum_purple) |>
            kableExtra::row_spec(nrow(tab), bold = TRUE, color = .sum_gold)
      } else {
         kb <- knitr::kable(tab, caption = "Per-stratum results and totals",
                            align = rep("r", 8), digits = digits, format = "html", escape = TRUE)
      }
      return(knitr::asis_output(paste(c(hdr, as.character(kb)), collapse = "\n")))
   }

   # other formats (PDF, Word): plain text header and pipe tables
   txt <- c("Stratification summary", "",
            sprintf("Strata (h): %s   n: %s   N: %s", h, x$n, if (is.null(N)) "NA" else N),
            sprintf("Data range: %s", rng_txt))
   if (x$cost && !is.null(x$ch))
      txt <- c(txt, sprintf("Stratum costs Ch: [%s]", paste(x$ch, collapse = ", ")))
   txt <- c(txt, sprintf("Best-fit distribution: %s", x$distr))
  txt <- c(txt[1:2], paste0(txt[-(1:2)], "  "), "")   # markdown line breaks
   txt <- c(txt, if (!is.null(pe))
      as.character(knitr::kable(pe, digits = 6, align = c("l", "r"),
                                caption = "Parameter estimate(s)"))
      else "(no parameter estimates)", "")
   kb <- knitr::kable(tab, caption = "Per-stratum results and totals",
                      align = rep("r", 8), digits = digits,
                      format = getOption("knitr.kable.format", default = "pipe"), escape = TRUE)
   knitr::asis_output(paste(c(txt, as.character(kb)), collapse = "\n"))
}
