#' Format and Present Results
#'
#' @param object A strata object.
#' @param ... Additional arguments.
#' 
#' Nicely formatted (and colored) summary for "strata" objects
#' Console: aligned ASCII table with crayon colors (if supported).
#' HTML: kable + kableExtra with yellow TOTAL row (text only, no background).
#' @export
summary.strata <- function(object, ...) {
   `%||%` <- function(a, b) if (!is.null(a)) a else b
   hrule <- function(char = "-", width = getOption("width", 80)) {
      paste(rep(char, max(40L, min(120L, width))), collapse = "")
   }
   
   ## --- color setup (no backgrounds) ------------------------------------------
   want_color <- isTRUE(getOption("stratifyR.color", TRUE))
   has_crayon <- want_color &&
      requireNamespace("crayon", quietly = TRUE) &&
      isTRUE(getOption("crayon.enabled", TRUE)) &&
      !isTRUE(as.logical(Sys.getenv("NO_COLOR", "0"))) &&
      isTRUE(try(crayon::has_color(), silent = TRUE))
   
   purple_hex <- "#5A189A"  # dark purple for OSB/WhSh (non-total)
   gold_text  <- "#CC9A00"  # yellow/gold text color for TOTAL row (and in HTML)
   
   if (has_crayon) {
      purple   <- crayon::make_style(purple_hex)
      yellowFG <- crayon::yellow         # console yellow (good contrast)
   }
   
   col_console <- function(x, role = NULL) {
      if (!has_crayon || is.null(role)) return(x)
      switch(
         role,
         title = crayon::bold(crayon::cyan(x)),
         key   = crayon::blue(x),
         val   = crayon::bold(crayon::cyan(x)),
         hdr   = crayon::green(x),
         osb   = crayon::combine_styles(purple, crayon::bold)(x),  # OSB purple
         whsh  = crayon::combine_styles(purple, crayon::bold)(x),  # WhSh purple
         tot   = crayon::combine_styles(yellowFG, crayon::bold)(x),# TOTAL row yellow text (bold)
         x
      )
   }
   
   ## --- knit/HTML detection ----------------------------------------------------
   in_knit <- isTRUE(getOption("knitr.in.progress"))
   is_html <- FALSE
   if (in_knit && requireNamespace("knitr", quietly = TRUE)) {
      is_html <- isTRUE(try(knitr::is_html_output(), silent = TRUE))
   }
   
   ## --- helpers ----------------------------------------------------------------
   fmt_params <- function(est, digits = 6, for_html = FALSE) {
      if (is.null(est) || length(est) == 0L || all(is.na(est))) return("(no parameter estimates)")
      nm <- names(est); if (is.null(nm)) nm <- paste0("par", seq_along(est))
      if (for_html) {
         return(data.frame(Parameter = nm, Estimate = as.numeric(est), check.names = FALSE))
      }
      key_w <- max(nchar(nm), 4L)
      vals <- vapply(est, function(v) if (is.na(v)) "NA" else formatC(v, digits=digits, format="g"),
                     FUN.VALUE = character(1))
      paste(sprintf(paste0("  %-", key_w, "s : %s"), nm, vals), collapse = "\n")
   }
   
   format_col <- function(v, name) {
      if (!is.numeric(v)) return(as.character(v))
      digits <- switch(name, OSB=2L, Wh=2L, Vh=2L, WhSh=3L, fh=2L, nh=0L, Nh=0L, 3L)
      if (digits == 0L) formatC(v, format="d", big.mark=",")
      else              formatC(v, digits=digits, format="f", big.mark=",")
   }
   
   # Console table (TOTAL row = yellow text, bold; no background)
   fmt_table_console <- function(df) {
      raw_cols <- lapply(seq_along(df), function(j) format_col(df[[j]], names(df)[j]))
      names(raw_cols) <- names(df)
      widths <- vapply(seq_along(raw_cols), function(j)
         max(nchar(names(df)[j]), nchar(raw_cols[[j]]), na.rm=TRUE), 0L)
      
      header_cells <- mapply(function(h, w) sprintf(paste0("%-", w, "s"), h),
                             names(df), widths, SIMPLIFY = FALSE)
      if (has_crayon) header_cells <- lapply(header_cells, col_console, role = "hdr")
      header <- paste(header_cells, collapse = "  ")
      
      nR <- nrow(df)
      rows <- vapply(seq_len(nR), function(i) {
         cells <- mapply(function(col, w, nm) {
            cell <- sprintf(paste0("%-", w, "s"), col[i])
            # normal rows: color OSB and WhSh only
            if (has_crayon && i < nR) {
               if (nm == "OSB")  cell <- col_console(cell, "osb")
               if (nm == "WhSh") cell <- col_console(cell, "whsh")
            }
            # TOTAL row: entire row yellow text (bold), including WhSh
            if (has_crayon && i == nR) {
               cell <- col_console(cell, "tot")
            }
            cell
         }, raw_cols, widths, names(df), SIMPLIFY = FALSE)
         paste(cells, collapse = "  ")
      }, FUN.VALUE = character(1))
      
      c(header, rows)
   }
   
   ## --- 1) Extract inputs ------------------------------------------------------
   cost    <- isTRUE(object$cost)
   distr   <- object$distr
   fit     <- object$fit
   n       <- object$n
   N       <- object$N
   ch      <- object$ch
   maxval  <- object$maxval
   initval <- object$initval
   finval  <- object$finval
   dist    <- object$dist
   h       <- nrow(object$h)
   
   param_est <- tryCatch({
      if (!is.null(fit$estimate)) fit$estimate else fit$fit$estimate
   }, error = function(e) NULL)
   
   ## --- 2) Build results table -------------------------------------------------
   # OSB: COBYLA returns h-1 interior boundaries; DP returns h values (interior +
   # final). Pad to exactly h elements so the data.frame row counts match.
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
   tab <- rbind(tab2, tab3)
   
   ## --- 3) Knit path (HTML; TOTAL row yellow text, no background) -------------
   if (in_knit && requireNamespace("knitr", quietly = TRUE)) {
      rng_txt <- sprintf("[%.4f, %.4f]  (d = %.4f)",
                         maxval * initval, maxval * finval, maxval * dist)
      hdr <- character(0)
      if (is_html) {
         hdr <- utils::capture.output({
         cat("<div style='margin-bottom:0.5rem'><span style='color:#0b7285;font-weight:700'>Stratification summary</span></div>\n", sep = "")
         cat(sprintf("<div><span style='color:#0b7285;font-weight:600'>Strata (h):</span> %s &nbsp; ", h))
         cat(sprintf("<span style='color:#0b7285;font-weight:600'>n:</span> %s &nbsp; ", n))
         cat(sprintf("<span style='color:#0b7285;font-weight:600'>N:</span> %s</div>\n", if (is.null(N)) "NA" else N))
         cat(sprintf("<div><span style='color:#0b7285;font-weight:600'>Data range:</span> %s</div>\n", rng_txt))
         if (cost && !is.null(ch)) {
            cat(sprintf("<div><span style='color:#0b7285;font-weight:600'>Stratum costs Ch:</span> [%s]</div>\n",
                        paste(ch, collapse = ", ")))
         }
         cat(sprintf("<div><span style='color:#0b7285;font-weight:600'>Best-fit distribution:</span> %s</div>\n\n", distr))
         })
      } else {
         cat("Stratification summary\n\n")
         cat(sprintf("Strata (h): %s   n: %s   N: %s\n", h, n, if (is.null(N)) "NA" else N))
         cat(sprintf("Data range: %s\n", rng_txt))
         if (cost && !is.null(ch)) cat(sprintf("Stratum costs Ch: [%s]\n", paste(ch, collapse = ", ")))
         cat(sprintf("Best-fit distribution: %s\n\n", distr))
      }
      
      # parameter estimates
      if (!is.null(param_est) && length(param_est) > 0L && !all(is.na(param_est))) {
         pe <- data.frame(Parameter = names(param_est),
                          Estimate  = as.numeric(param_est),
                          check.names = FALSE)
         if (is_html)
            hdr <- c(hdr, as.character(knitr::kable(pe, format = "html", digits = 6,
                                                    align = c("l","r"),
                                                    caption = "Parameter estimate(s)")))
         else
            print(knitr::kable(pe, digits = 6, align = c("l","r"), caption = "Parameter estimate(s)"))
      } else {
         if (is_html) hdr <- c(hdr, "<p>(no parameter estimates)</p>")
         else cat("(no parameter estimates)\n\n")
      }
      
      digits <- c(0, 2, 2, 2, 3, 0, 0, 2)
      
      # HTML: style the TOTAL row text as gold (no background),
      # keep OSB & WhSh columns purple in non-total rows.
      tab_html <- tab
      esc <- TRUE
      if (is_html && requireNamespace("kableExtra", quietly = TRUE)) {
         # Pre-format non-total OSB/WhSh if you want color in those columns:
         # (We keep column-level color instead-see column_spec below.)
         
         kb <- knitr::kable(
            tab_html,
            caption = "Per-stratum results and totals",
            align   = c("r","r","r","r","r","r","r","r"),
            digits  = digits,
            format  = "html",
            escape  = TRUE
         ) |>
            kableExtra::kable_styling(full_width = FALSE, bootstrap_options = c("condensed")) |>
           kableExtra::row_spec(0, bold = TRUE, color = "#2b8a3e") |>      # header green
           kableExtra::column_spec(2, color = purple_hex) |>               # OSB purple
           kableExtra::column_spec(5, color = purple_hex) |>               # WhSh purple
           kableExtra::row_spec(nrow(tab_html), bold = TRUE, color = gold_text) # TOTAL row yellow text
         
         # v2.0-3: return the header, parameters and table as one HTML block,
         # so they render as HTML rather than as escaped text in a document
         return(knitr::asis_output(paste(c(hdr, as.character(kb)), collapse = "\n")))
      }
      
      # Non-HTML knit fallback
      kb <- knitr::kable(
         tab_html,
         caption = "Per-stratum results and totals",
         align   = c("r","r","r","r","r","r","r","r"),
         digits  = digits,
         format  = if (is_html) "html" else getOption("knitr.kable.format", default = "pipe"),
         escape  = TRUE
      )
      if (is_html)
         return(knitr::asis_output(paste(c(hdr, as.character(kb)), collapse = "\n")))
      return(kb)
   }
   
   ## --- Console path -----------------------------------------------------------
   cat(hrule("="), "\n", sep = "")
   cat(col_console("Stratification summary", "title"), "\n", sep = "")
   cat(hrule("-"), "\n", sep = "")
   
   rng_txt <- sprintf("[%.4f, %.4f]  (d = %.4f)",
                      maxval * initval, maxval * finval, maxval * dist)
   cat(col_console("Strata (h): ", "key"), col_console(h, "val"), "    ",
       col_console("n: ", "key"),       col_console(n, "val"), "    ",
       col_console("N: ", "key"),       col_console(if (is.null(N)) "NA" else as.character(N), "val"),
       "\n", sep = "")
   cat(col_console("Data range: ", "key"), rng_txt, "\n", sep = "")
   if (cost && !is.null(ch)) {
      cat(col_console("Stratum costs Ch: ", "key"), paste0("[", paste(ch, collapse = ", "), "]"), "\n", sep = "")
   }
   cat(col_console("Best-fit distribution: ", "key"), distr, "\n", sep = "")
   if (isTRUE(object$takeall > 0))
      cat(col_console("Take-all stratum: ", "key"),
          sprintf("stratum %d (%s units, sampled completely)", h,
                  format(object$takeall, big.mark = ",")), "\n", sep = "")
   if (isTRUE(object$fpc) && !is.null(object$variance))
      cat(col_console("Variance of the mean (with fpc): ", "key"),
          formatC(object$variance, digits = 6, format = "g"), "\n", sep = "")
   
   cat(col_console("Parameter estimate(s):", "key"), "\n", sep = "")
   cat(fmt_params(try(if (!is.null(fit$estimate)) fit$estimate else fit$fit$estimate, silent = TRUE)), "\n", sep = "")
   
   cat(hrule("-"), "\n", sep = "")
   cat(paste(fmt_table_console(tab), collapse = "\n"), "\n", sep = "")
   cat(hrule("="), "\n", sep = "")
   
   invisible(tab)
}
