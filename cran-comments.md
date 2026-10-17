## Submission

This is an update of stratifyR from 2.0-2 to 2.0-3. I apologise for sending
it soon after 2.0-2. It is a correctness release that fixes errors found in a
larger simulation study after 2.0-2 was accepted; some of them give wrong
results, so I did not want to leave them on CRAN. The user interface is
unchanged. The main fixes are:

* Pareto and triangular distribution fits failed inside the package, so a
  wrong family could be selected; on some platforms a numerically unstable
  Pareto likelihood could also make the Pareto win the AIC comparison.
* method = "cobyla" and "global" could leave the smallest or largest unit out
  of the cost they minimise, giving designs far from the optimum on data with
  extreme values.
* method = "dp" could stop with an error on some heavy-tailed data.
* strata.distr() stratum sizes did not always add up to N.

Each fix has a unit test. Details are in NEWS.md.

## Test environments

* local macOS 27.0.1 (aarch64-apple-darwin23), R 4.6.1

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies on CRAN.
