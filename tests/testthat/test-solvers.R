# Brute-force reference: smallest sum(W_h * S_h) over all contiguous partitions
brute_phi <- function(x, h) {
  x <- sort(x); N <- length(x); best <- Inf
  cuts <- utils::combn(2:(N - 2), h - 1)
  for (k in seq_len(ncol(cuts))) {
    e <- c(0, cuts[, k], N)
    if (any(diff(e) < 2)) next
    phi <- sum(vapply(seq_len(h), function(i) {
      s <- x[(e[i] + 1):e[i + 1]]; length(s) / N * stats::sd(s) }, numeric(1)))
    best <- min(best, phi)
  }
  best
}

test_that("method = 'exact' matches full enumeration", {
  set.seed(1)
  x <- rlnorm(30)
  for (h in 2:3) {
    r <- suppressWarnings(suppressMessages(strata.data(x, h = h, n = 10, method = "exact")))
    expect_equal(r$WhShTot, round(brute_phi(x, h), 6), tolerance = 1e-6)
  }
})

test_that("every unit is counted exactly once by every solver", {
  set.seed(2)
  x <- rgamma(400, shape = 2)
  for (m in c("exact", "cobyla", "global")) {
    r <- suppressWarnings(suppressMessages(strata.data(x, h = 4, n = 80, method = m)))
    expect_equal(sum(r$Nh), length(x))
    expect_length(r$OSB, 4)
    expect_equal(r$OSB[4], max(x))
  }
})

test_that("continuous solvers can place narrow strata in a heavy tail", {
  set.seed(3)
  x <- rlnorm(1500, 0, 1)
  ex <- suppressWarnings(suppressMessages(strata.data(x, h = 4, n = 200, method = "exact")))
  cb <- suppressWarnings(suppressMessages(strata.data(x, h = 4, n = 200, method = "cobyla")))
  expect_lt((cb$WhShTot - ex$WhShTot) / ex$WhShTot, 0.02)
  expect_gte(cb$WhShTot, ex$WhShTot - 1e-6)
})

test_that("solvers leave the user's random number stream unchanged", {
  set.seed(4); x <- rexp(300); a <- runif(1)
  set.seed(4); x <- rexp(300)
  invisible(suppressWarnings(suppressMessages(
    strata.data(x, h = 3, n = 50, method = "cobyla")))); b <- runif(1)
  expect_identical(a, b)
})

test_that("stratum sample sizes are whole numbers that add up to n", {
  data(anaemia, package = "stratifyR", envir = environment())
  for (h in 2:5) {
    r <- suppressWarnings(suppressMessages(
      strata.data(anaemia$Iron, h = h, n = 300, method = "exact")))
    expect_equal(sum(r$nh), 300)
    expect_equal(r$nhTot, 300)
    expect_true(all(r$nh == round(r$nh)))
    expect_true(all(r$nh <= r$Nh))
  }
})

test_that("exact: reported cost matches its own boundaries on heavy-tailed data", {
  # Boundaries fall exactly on data values; the allocation step must assign
  # each boundary unit to the lower stratum on every platform.
  set.seed(42)
  x <- rcauchy(3000, location = 5, scale = 1)
  cost <- function(b) {
    g <- findInterval(x, b, left.open = TRUE) + 1L
    sum(tapply(x, g, function(v) length(v) / length(x) * stats::sd(v)))
  }
  for (h in 3:4) {
    r <- suppressWarnings(suppressMessages(strata.data(x, h = h, n = 300, method = "exact")))
    expect_equal(r$WhShTot, round(cost(as.numeric(r$OSB)[seq_len(h - 1)]), 6), tolerance = 1e-6)
    cb <- suppressWarnings(suppressMessages(strata.data(x, h = h, n = 300, method = "cobyla")))
    expect_gte(cb$WhShTot, r$WhShTot - 1e-6)
  }
})

test_that("cobyla copes with an infinite-variance (very wide range) sample", {
  set.seed(6)
  x <- actuar::rpareto(3000, shape = 2, scale = 1)     # range in the hundreds
  ex <- suppressWarnings(suppressMessages(strata.data(x, h = 4, n = 300, method = "exact")))
  cb <- suppressWarnings(suppressMessages(strata.data(x, h = 4, n = 300, method = "cobyla")))
  expect_lt((cb$WhShTot - ex$WhShTot) / ex$WhShTot, 0.02)
})

test_that("GLOBAL and COBYLA report the cost of the partition they found (tied data)", {
  data(math, package = "stratifyR")
  x <- math$final_marks
  ex <- suppressMessages(strata.data(x, h = 3, n = 243, method = "exact"))$WhShTot
  gl <- suppressMessages(strata.data(x, h = 3, n = 243, method = "global"))$WhShTot
  cb <- suppressMessages(strata.data(x, h = 3, n = 243, method = "cobyla"))$WhShTot
  expect_lt(gl / ex - 1, 1e-8)
  expect_lt(cb / ex - 1, 1e-8)
})

test_that("strata.distr() stops when the range lies outside a bounded support", {
  expect_error(strata.distr(h = 2, initval = 1, dist = 0.9, distr = "rtriangle",
                            params = c(min = 2, max = 10, mode = 2), n = 50, N = 100),
               "outside the support")
})

test_that("COBYLA and GLOBAL keep the extreme units in the outer strata (two-sided heavy tails)", {
  set.seed(1)
  x <- rcauchy(5000, location = 5, scale = 1)
  for (h in c(3, 6)) {
    ex <- suppressMessages(strata.data(x, h = h, n = 500, method = "exact"))$WhShTot
    cb <- suppressMessages(strata.data(x, h = h, n = 500, method = "cobyla"))$WhShTot
    expect_lt(cb / ex - 1, 1e-3)
  }
})

test_that("the Pareto likelihood stays accurate at extreme parameter values", {
  set.seed(3); x <- runif(5000, 0, 10)
  r <- suppressMessages(strata.data(x, h = 2, n = 500, method = "exact"))
  expect_equal(r$distr, "unif")
})

test_that("strata.distr() stratum sizes add up to N", {
  g <- suppressMessages(strata.distr(h = 4, initval = 500, dist = 119500, distr = "lnorm",
         params = c(meanlog = 9.616, sdlog = 0.703), n = 1000, N = 20000))
  expect_equal(sum(g$Nh), 20000)
  expect_equal(sum(g$nh), 1000)
})
