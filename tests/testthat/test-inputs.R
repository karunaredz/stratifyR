# Input checks (added in 2.1-0): every bad input stops with a message that
# names the argument and says what is expected. These checks run before any
# fitting, so the tests are fast.

set.seed(10)
x <- rgamma(200, shape = 2)

test_that("strata.data() checks h", {
  expect_error(strata.data(x, h = 1, n = 50), "'h' must be a whole number of at least 2")
  expect_error(strata.data(x, h = 2.5, n = 50), "'h' must be a whole number")
  expect_error(strata.data(x, h = c(2, 3), n = 50), "'h' must be a whole number")
  expect_error(strata.data(x, h = "3", n = 50), "'h' must be a whole number")
  expect_error(strata.data(x, n = 50), "'h' \\(the number of strata\\) must be given")
})

test_that("strata.data() checks n", {
  expect_error(strata.data(x, h = 3, n = 0), "'n' must be a whole number of at least 1")
  expect_error(strata.data(x, h = 3, n = 2), "smaller than 'h'")
  expect_error(strata.data(x, h = 3, n = 500), "larger than the population size")
  expect_error(strata.data(x, h = 3, n = 50.5), "'n' must be a whole number")
  expect_error(strata.data(x, h = 3), "'n' \\(the total sample size\\) must be given")
})

test_that("strata.data() checks data", {
  expect_error(strata.data(c(x, NA), h = 3, n = 50), "missing value")
  expect_error(strata.data(c(x, Inf), h = 3, n = 50), "infinite")
  expect_error(strata.data(as.character(x), h = 3, n = 50), "must be numeric")
  expect_error(strata.data(factor(1:20), h = 3, n = 10), "must be numeric")
  expect_error(strata.data(data.frame(a = x), h = 3, n = 50), "not a data frame")
  expect_error(strata.data(rep(5, 100), h = 3, n = 50), "All values in 'data' are the same")
  expect_error(strata.data(x[1:4], h = 3, n = 3), "need at least 6")
  expect_error(strata.data(rep(1:2, 20), h = 3, n = 10), "only 2 distinct values")
})

test_that("strata.data() checks costs, method and options", {
  expect_error(strata.data(x, h = 3, n = 50, cost = TRUE), "'ch' .* must be given")
  expect_error(strata.data(x, h = 3, n = 50, cost = TRUE, ch = c(1, -2, 3)),
               "3 positive numbers")
  expect_error(strata.data(x, h = 3, n = 50, cost = TRUE, ch = c(1, 2)),
               "3 positive numbers")
  expect_error(strata.data(x, h = 3, n = 50, cost = "yes"), "'cost' must be TRUE or FALSE")
  expect_error(strata.data(x, h = 3, n = 50, method = "exactt"), "'method' must be one of")
  expect_error(strata.data(x, h = 3, n = 50, fpc = TRUE), "method = \"exact\" only")
  expect_error(strata.data(x, h = 3, n = 50, method = "exact", fpc = "yes"),
               "'fpc' must be TRUE or FALSE")
  expect_error(strata.data(x, h = 3, n = 50, method = "cobyla", n_starts = 0),
               "'n_starts' must be a whole number")
  expect_error(strata.data(x, h = 3, n = 50, method = "cobyla", tol = -1),
               "'tol' must be a single positive number")
})

test_that("abbreviated method names still work", {
  r <- suppressMessages(strata.data(x, h = 2, n = 20, method = "ex"))
  expect_identical(r$method, "exact")
})

test_that("a valid call gives no warnings", {
  expect_no_warning(suppressMessages(strata.data(x, h = 3, n = 50, method = "exact")))
})

test_that("strata.distr() checks its inputs", {
  ok <- list(h = 3, initval = 0.5, dist = 10, distr = "gamma",
             params = c(shape = 2, rate = 1), n = 200, N = 5000)
  call_with <- function(...) {
    a <- utils::modifyList(ok, list(...))
    do.call(strata.distr, a)
  }
  expect_error(call_with(distr = "gama"), "is not supported")
  expect_error(call_with(params = c(shape = 2)), "Missing: rate")
  expect_error(call_with(distr = "norm", params = c(mean = 16, sd = -1)),
               "sd must be positive")
  expect_error(call_with(distr = "unif", params = c(min = 5, max = 1)),
               "min smaller than max")
  expect_error(call_with(distr = "triangle", params = c(min = 0, max = 10, mode = 12)),
               "mode between min and max")
  expect_error(call_with(dist = 0), "'dist' must be a single positive number")
  expect_error(call_with(n = 6000), "larger than the population size")
  expect_error(call_with(h = 1), "'h' must be a whole number of at least 2")
  expect_error(call_with(method = "exact"), "'method' must be one of")
  a <- ok; a$N <- NULL
  expect_error(do.call(strata.distr, a), "'N' \\(the population size\\) must be given")
})

test_that("compare_designs() checks its inputs", {
  r <- suppressMessages(strata.data(x, h = 3, n = 50, method = "exact"))
  expect_error(compare_designs(r, n = -5), "'n' must be a whole number")
  expect_error(compare_designs(r, n = 1000), "larger than the population size")
  expect_error(compare_designs(list(a = 1)), "needs a result from strata.data")
})
