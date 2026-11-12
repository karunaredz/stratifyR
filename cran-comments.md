## Submission

This is an update of stratifyR from 2.0-4 to 2.1-0. It improves the design of
the package following comments from journal editors. Existing calls give the
same numerical results as in 2.0-4. The changes are:

* New accessor functions boundaries(), allocation(), objective(), variance()
  and fitted_distribution(), and as.data.frame() methods, so results can be
  used without accessing the internal structure of the objects.
* summary() now returns an object of class "summary.strata" with print(),
  as.data.frame() and knit_print() methods, instead of printing as a side
  effect. The printed output is unchanged.
* All user-facing functions check their arguments and give clear messages.
* Internal helper functions are marked with keyword internal, and help page
  titles are in sentence case.
* A valid call no longer gives spurious "NaNs produced" warnings.

The new functions and checks have unit tests. Details are in NEWS.md.

## Test environments

* local Linux (x86_64), R 4.3.3
<!-- add after running: local macOS (aarch64-apple-darwin23), R 4.6.1; win-builder (R-devel) -->

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies on CRAN.
