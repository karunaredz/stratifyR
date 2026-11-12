# =============================================================================
# checks.R  -  stratifyR 2.1
#
# Input checks for the user-facing functions. Each check stops with a plain
# message that says which argument is wrong and what is expected.
# =============================================================================

.stop <- function(...) stop(paste0(...), call. = FALSE)

.is_whole <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x) &&
  abs(x - round(x)) < 1e-8

.check_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x))
    .stop("'", name, "' must be TRUE or FALSE.")
  invisible(x)
}

.check_count <- function(x, name, min = 1L, what = "") {
  if (!.is_whole(x) || x < min)
    .stop("'", name, "' must be a whole number of at least ", min, what,
          "; you gave ", .show(x), ".")
  as.integer(round(x))
}

.check_positive <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0)
    .stop("'", name, "' must be a single positive number; you gave ", .show(x), ".")
  invisible(x)
}

.show <- function(x) {
  if (is.null(x)) return("NULL")
  if (is.character(x)) x <- paste0("\"", x, "\"")
  s <- paste(utils::head(x, 4), collapse = ", ")
  if (length(x) > 4) s <- paste0(s, ", ...")
  if (length(x) != 1L) s <- paste0("c(", s, ")")
  s
}

# h: number of strata
.check_h <- function(h) {
  if (missing(h) || is.null(h))
    .stop("'h' (the number of strata) must be given.")
  .check_count(h, "h", min = 2L, what = " (the number of strata)")
}

# n: total sample size, given h and (optionally) N
.check_n <- function(n, h, N = NULL) {
  if (missing(n) || is.null(n))
    .stop("'n' (the total sample size) must be given.")
  n <- .check_count(n, "n", min = 1L, what = " (the total sample size)")
  if (n < h)
    .stop("'n' = ", n, " is smaller than 'h' = ", h,
          ". The sample needs at least one unit in each stratum.")
  if (!is.null(N) && n > N)
    .stop("'n' = ", n, " is larger than the population size N = ", N, ".")
  n
}

# cost and ch: stratum costs
.check_cost <- function(cost, ch, h) {
  .check_flag(cost, "cost")
  if (isTRUE(cost)) {
    if (is.null(ch))
      .stop("'ch' (the cost per unit in each stratum) must be given when cost = TRUE.")
    if (!is.numeric(ch) || length(ch) != h || any(!is.finite(ch)) || any(ch <= 0))
      .stop("'ch' must be ", h, " positive numbers, one cost per stratum; you gave ",
            .show(ch), ".")
  } else if (!is.null(ch)) {
    warning("'ch' is ignored because cost = FALSE. Set cost = TRUE to use stratum costs.",
            call. = FALSE)
  }
  invisible(TRUE)
}

# settings of the continuous solvers
.check_solver_settings <- function(n_starts, max_iter, tol, verbose) {
  .check_count(n_starts, "n_starts", min = 1L)
  .check_count(max_iter, "max_iter", min = 1L)
  .check_positive(tol, "tol")
  .check_flag(verbose, "verbose")
  invisible(TRUE)
}

# data: the stratification variable for strata.data()
.check_data <- function(data, h) {
  if (missing(data) || is.null(data))
    .stop("'data' (the values of the stratification variable) must be given.")
  if (is.data.frame(data) || is.matrix(data))
    .stop("'data' must be a numeric vector, not a ",
          if (is.data.frame(data)) "data frame" else "matrix",
          ". Pass one column, for example data = mydata$income.")
  if (is.factor(data) || !is.numeric(data))
    .stop("'data' must be numeric; you gave an object of class '",
          class(data)[1], "'.")
  if (any(is.na(data)))
    .stop("'data' has ", sum(is.na(data)), " missing value(s). ",
          "Remove them first, for example with data = na.omit(x).")
  if (any(!is.finite(data)))
    .stop("'data' has infinite values. Remove them first.")
  N <- length(data)
  if (N < 2L * h)
    .stop("'data' has ", N, " values, but ", h, " strata need at least ",
          2L * h, " (two units in each stratum).")
  nd <- length(unique(data))
  if (nd < 2L)
    .stop("All values in 'data' are the same, so the population cannot be stratified.")
  if (nd < h)
    .stop("'data' has only ", nd, " distinct values, fewer than the ", h,
          " strata asked for.")
  invisible(N)
}

# distribution name and parameters for strata.distr()
.distr_params <- list(
  pareto    = c("shape", "scale"),
  triangle  = c("min", "max", "mode"),
  rtriangle = c("min", "max"),
  weibull   = c("shape", "scale"),
  gamma     = c("shape", "rate"),
  exp       = c("rate"),
  unif      = c("min", "max"),
  norm      = c("mean", "sd"),
  lnorm     = c("meanlog", "sdlog"),
  cauchy    = c("location", "scale")
)

.check_distr <- function(distr, params, initval, dist) {
  if (!is.character(distr) || length(distr) < 1L || is.na(distr[1]))
    .stop("'distr' must be the name of a distribution, one of: ",
          paste0("\"", names(.distr_params), "\"", collapse = ", "), ".")
  d <- distr[1]
  if (!d %in% names(.distr_params))
    .stop("'distr' = \"", d, "\" is not supported. Use one of: ",
          paste0("\"", names(.distr_params), "\"", collapse = ", "), ".")
  need <- .distr_params[[d]]
  p <- unlist(params)
  if (!is.numeric(p) || is.null(names(p)))
    .stop("'params' must be a named numeric vector or list, for example ",
          "params = c(", paste0(need, " = ...", collapse = ", "), ").")
  miss <- setdiff(need, names(p))
  if (length(miss))
    .stop("For distr = \"", d, "\", 'params' must include ",
          paste(need, collapse = " and "), ". Missing: ",
          paste(miss, collapse = ", "), ".")
  bad <- need[!is.finite(p[need])]
  if (length(bad))
    .stop("'params' has a missing or infinite value for ", paste(bad, collapse = ", "), ".")
  pos <- intersect(need, c("shape", "scale", "rate", "sd", "sdlog"))
  neg <- pos[p[pos] <= 0]
  if (length(neg))
    .stop("For distr = \"", d, "\", ", paste(neg, collapse = " and "),
          " must be positive.")
  if (d %in% c("unif", "triangle", "rtriangle") && p[["min"]] >= p[["max"]])
    .stop("'params' must have min smaller than max.")
  if (d == "triangle" && (p[["mode"]] < p[["min"]] || p[["mode"]] > p[["max"]]))
    .stop("'params' must have mode between min and max.")
  if (!is.numeric(initval) || length(initval) != 1L || !is.finite(initval))
    .stop("'initval' (the smallest value of the variable) must be a single number.")
  .check_positive(dist, "dist")
  invisible(d)
}

# method: one of the solvers
.check_method <- function(method, choices) {
  if (identical(method, choices)) return(choices[1])   # default not changed
  m <- if (is.character(method) && length(method) == 1L && !is.na(method))
    pmatch(method, choices) else NA_integer_
  if (is.na(m))
    .stop("'method' must be one of ", paste0("\"", choices, "\"", collapse = ", "),
          "; you gave ", .show(method), ".")
  choices[m]   # abbreviations such as "cob" still work, as with match.arg()
}
