## Submission

This is an update of stratifyR from 2.0-4 to 2.1-0. It improves the design of
the package following comments from journal editors. Existing calls give the
same numerical results as in 2.0-4. The changes are:

* New accessor functions boundaries(), allocation(), objective(), variance()
  and fitted_distribution(), and as.data.frame() methods, so results can be
  used without accessing the internal structure of the objects.
* summary() now returns an object of class "summary.stratifyR" with print(),
  as.data.frame() and knit_print() methods, instead of printing as a side
  effect. The printed output is unchanged.
* Results now have class c("stratifyR", "strata"), and the S3 methods are
  registered for "stratifyR". Before, the class was "strata", the same class
  name used by the CRAN package stratification, so loading both packages made
  one package's print() and plot() methods replace the other's. Code that
  tests inherits(x, "strata") still works.
* All user-facing functions check their arguments and give clear messages.
* Internal helper functions are marked with keyword internal, and help page
  titles are in sentence case.
* A valid call no longer gives spurious "NaNs produced" warnings.
* A mislabelled value in the Shiny app is now labelled correctly.

The new functions and checks have unit tests. Details are in NEWS.md.

## Test environments

* local Linux (x86_64-pc-linux-gnu), R 4.3.3
* local macOS (aarch64-apple-darwin), R 4.6.1
* win-builder (R-devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies on CRAN.
