## Resubmission

This is a resubmission of 2.0-2. In this version I have:

* Fixed the invalid URLs: the GitHub repository given in URL and BugReports
  (https://github.com/karunaredz/stratifyR) is now public.
* Fixed a platform-specific rounding issue found after the first submission:
  with method = "exact", a stratum boundary rebuilt as (b / maxval) * maxval
  could round one bit low on Apple silicon and move one unit into the next
  stratum. The allocation step now uses a cut point halfway between data
  values, and a unit test covers this case.
* Distribution fitting no longer prints optimiser messages to the console, and
  the vignette has a new section on working with the results.

## Submission

This is an update of stratifyR from 2.0-1 to 2.0-2. The main changes are:

* a new solver, method = "exact", that finds the exact optimum strata
  boundaries for a given dataset by dynamic programming;
* bug fixes in the "cobyla" and "global" solvers, the DP boundary rounding,
  the assignment of units to strata and the rounding of stratum sample sizes;
* fixes to the bundled 'shiny' app and updated documentation and vignette.

Details are in NEWS.md. The user-facing interface is unchanged.

## Test environments

* local macOS 27.0.1 (aarch64-apple-darwin23), R 4.6.1

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies on CRAN.
