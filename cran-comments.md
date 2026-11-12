## Submission

This is an update of stratifyR from 2.0-3 to 2.0-4. I am sorry to send it soon
after 2.0-3. It adds two options that are needed for the results of the
accompanying paper that is about to be submitted to the Journal of Statistical
Software, and I would like the CRAN version to match the paper. Existing calls
give the same results as in 2.0-3. The changes are:

* method = "exact" in strata.data() has two new options. fpc = TRUE minimises
  the full variance of the stratified mean, including the finite population
  correction. takeall = TRUE also allows a take-all top stratum when this
  lowers the variance. Both default to FALSE.
* strata.data() results have three new elements: variance, takeall and fpc.
  summary() shows them when the options are used, and a warning is given
  when a stratum gets fewer than 2 sample units.
* The Shiny app (inst/app) has controls for the new options, and its labels
  now write the number of strata as L.
* The vignette has a new section on the new options.

The new options have unit tests, including a check of fpc = TRUE against a
full enumeration of a small sample. Details are in NEWS.md.

## Test environments

* local macOS 27.0.1 (aarch64-apple-darwin23), R 4.6.1
* win-builder (R-devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies on CRAN.
