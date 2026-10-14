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
