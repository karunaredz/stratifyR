# summary() class and accessor functions (added in 2.1-0)

set.seed(20)
x <- rlnorm(300, meanlog = 2, sdlog = 0.6)
res <- suppressMessages(strata.data(x, h = 4, n = 60, method = "exact"))

test_that("summary() returns a summary.stratifyR object and prints nothing", {
  out <- utils::capture.output(s <- summary(res))
  expect_length(out, 0L)
  expect_s3_class(s, "summary.stratifyR")
  expect_equal(s$L, 4L)
  tab <- as.data.frame(s)
  expect_s3_class(tab, "data.frame")
  expect_equal(nrow(tab), 5L)                    # 4 strata and a total row
  expect_identical(tab$Strata[5], "Total")
})

test_that("printing the summary shows the table once and returns it invisibly", {
  s <- summary(res)
  out <- utils::capture.output(r <- withVisible(print(s)))
  expect_false(r$visible)
  expect_identical(r$value, s)
  expect_equal(sum(grepl("^Total", out)), 1L)
})

test_that("boundaries() gives the cut points, with or without the ends", {
  b <- boundaries(res)
  expect_length(b, 3L)
  expect_true(all(diff(b) > 0))
  be <- boundaries(res, ends = TRUE)
  expect_length(be, 5L)
  expect_equal(be[2:4], b)
  expect_equal(be[1], min(x), tolerance = 1e-8)
  expect_equal(be[5], max(x), tolerance = 1e-8)
  expect_error(boundaries(res, ends = "yes"), "'ends' must be TRUE or FALSE")
})

test_that("allocation() agrees with the data and with objective()", {
  a <- allocation(res)
  expect_named(a, c("stratum", "lower", "upper", "Nh", "Wh", "Sh", "WhSh", "nh", "fh"))
  expect_equal(sum(a$Nh), length(x))
  expect_equal(sum(a$nh), 60)
  expect_equal(sum(a$WhSh), objective(res), tolerance = 1e-10)
  # each stratum's values match the data directly
  xs <- sort(x); grp <- findInterval(xs, boundaries(res), left.open = TRUE) + 1L
  expect_equal(a$Nh, as.numeric(tabulate(grp, 4L)))
  expect_equal(a$Sh, as.numeric(tapply(xs, grp, stats::sd)), tolerance = 1e-10)
  expect_identical(as.data.frame(res), a)
})

test_that("objective() is the minimised sum of W_h S_h", {
  expect_equal(objective(res), res$WhShTot, tolerance = 1e-6)
  expect_true(objective(res) > 0)
})

test_that("fitted_distribution() names the distribution and its estimates", {
  f <- fitted_distribution(res)
  expect_named(f, c("distr", "estimate"))
  expect_type(f$distr, "character")
  expect_true(is.numeric(f$estimate) && length(f$estimate) >= 1L)
})

test_that("accessors work for strata.distr() results", {
  r <- suppressMessages(strata.distr(h = 3, initval = 0.5, dist = 10, distr = "gamma",
                                     params = c(shape = 2, rate = 1), n = 200, N = 5000))
  expect_length(boundaries(r), 2L)
  expect_equal(boundaries(r, ends = TRUE)[c(1, 4)], c(0.5, 10.5))
  a <- allocation(r)
  expect_equal(sum(a$Nh), 5000)
  expect_equal(sum(a$WhSh), objective(r), tolerance = 1e-10)
})

test_that("compare_designs() results have as.data.frame() and objective()", {
  cd <- compare_designs(res)
  d <- as.data.frame(cd)
  expect_identical(d$design, c("SRS", "Proportional", "Neyman"))
  expect_equal(d$deff[1], 1)
  expect_true(all(d$variance > 0))
  expect_true(d$variance[3] <= d$variance[2])    # Neyman no worse than proportional
  expect_equal(objective(cd), res$WhShTot, tolerance = 1e-6)
})

test_that("objects saved by older versions still work with the accessors", {
  old <- res
  old$stratum_full <- NULL; old$objective_full <- NULL
  expect_equal(objective(old), res$WhShTot)
  expect_equal(nrow(allocation(old)), 4L)
})

test_that("variance() is the variance of the mean with the fpc", {
  a <- allocation(res)
  s <- a$nh > 0 & a$nh < a$Nh
  v <- sum((a$Wh^2 * a$Sh^2 * (1 / a$nh - 1 / a$Nh))[s])
  expect_equal(variance(res), v, tolerance = 1e-12)
  r <- suppressMessages(strata.distr(h = 3, initval = 0.5, dist = 10, distr = "gamma",
                                     params = c(shape = 2, rate = 1), n = 200, N = 5000))
  expect_true(is.finite(variance(r)) && variance(r) > 0)
})

test_that("results keep their own methods when the stratification package is loaded", {
  skip_if_not_installed("stratification")
  x <- c(1:40, 60, 80, 100, 150, 300)
  res <- suppressMessages(strata.data(x, h = 3, n = 20, method = "exact"))
  expect_s3_class(res, "stratifyR")
  expect_true(inherits(res, "strata"))
  loadNamespace("stratification")
  expect_output(print(res))
  f <- tempfile(fileext = ".png"); png(f); on.exit({dev.off(); unlink(f)}, add = TRUE)
  expect_error(suppressWarnings(plot(res)), NA)
  expect_s3_class(summary(res), "summary.stratifyR")
})
