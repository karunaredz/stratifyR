# stratifyR 2.0-2  (2026-10-14)

## New solver

* `strata.data(..., method = "exact")` runs Dynamic Programming on the data
  themselves. It returns the partition with the smallest empirical Neyman cost
  over all partitions into `h` contiguous strata with at least two units each,
  never splitting tied values. It is exact for data with up to 20,000 distinct
  cut points (about 15 to 20 seconds for 14,000 units at `h = 6`); beyond that
  it searches 20,000 cut points spread evenly in rank, which can be changed
  with `options(stratifyR.exact_max_cand = )`. The result records this in
  `exact_full`. The `"dp"` solver remains the default: it is globally optimal
  on a fine grid for the fitted distribution.

## Bug fixes (all solvers)

* The DP solver rounded its boundaries to two decimals, and the top boundary
  could round below the largest value, which then fell outside every stratum
  (for example 4,999 of 5,000 units counted). Boundaries are now kept at full
  precision and the top boundary is always the data maximum. Reported Neyman
  costs from 2.0-1 could be up to about 1 percent too low because of this.
* `method = "exact"` places each boundary exactly on a data value. The
  allocation step rebuilt it as `(b / maxval) * maxval`, which on some
  platforms (for example Apple silicon) can round one bit below `b` and move
  the boundary unit into the next stratum, so the reported cost could be a
  little above the true optimum (0.25% for a Cauchy sample at `h = 3`). The
  allocation step now uses a cut point halfway to the next data value.
* `data.alloc()` treated each stratum as closed at both ends, so a value equal
  to a boundary was counted in two strata. Every unit is now assigned to
  exactly one stratum, (b[h-1], b[h]].
* `WhShTot` is now reported to six decimals instead of three.
* Stratum sample sizes are now rounded with the largest-remainder rule, so
  they always add up to `n` and never exceed `N_h`. Rounding each `n_h` on its
  own could give a total of `n + 1` or `n - 1` (for example 63 + 103 + 135 =
  301 for the anaemia Iron data at `h = 3`, `n = 300`).
* Every solver now returns `h` boundaries in `OSB`, the last being the top of
  the range.

## Bug fixes (continuous solvers)

* `method = "cobyla"` and `method = "global"` no longer force every stratum to
  be at least `(range)/(3h)` wide. That rule excluded the narrow strata that
  skewed and heavy-tailed populations need, so on Pareto, log-normal and Cauchy
  data the two solvers returned designs with up to several times the optimal
  Neyman cost. Boundaries now only need to be `range/1000` apart, as in the
  problem formulation.
* The degeneracy check no longer treats a narrow stratum as a failure and no
  longer silently switches to the DP solver. A solution is rejected only if a
  stratum holds fewer than two units.
* `method = "global"` no longer accepts a result whose objective is the
  infeasibility penalty (a design with empty strata). If the global phase finds
  no feasible point, it falls back to multi-start COBYLA.
* The random restarts no longer change the user's random number stream.
* `method = "global"` no longer reports `converged = FALSE` when the BOBYQA
  refinement confirms the DIRECT-L point without improving it.

## Shiny app

* The strata-count (H) selector and the "Generate R code" button no longer pass
  an `N` argument to `strata.data()`, which has none. Previously this made the
  H curve drop points and the generated code fail when a population size was set.
* Solver labels now describe GLOBAL as DIRECT-L followed by local refinement
  (the refinement step uses BOBYQA).
* New stratifyR hex logo in the app header (and as the browser tab icon),
  and the version badge now reads the installed package version instead of
  a fixed "2.0-1".
* New EXACT solver option for data sources. With an assumed distribution the
  app uses DP, which is exact for that distribution.
* Help texts no longer describe COBYLA as unreliable on heavy tails; that was
  caused by the minimum-width rule fixed above.

## Vignette

* Section 7 is rewritten for the four solvers (DP, COBYLA, GLOBAL and EXACT),
  with a guide to choosing one and worked examples that include
  `method = "exact"`.
* New Section 8, "Working with the Results": reading the output, choosing the
  number of strata, `compare_designs()`, cost-aware allocation, plots and
  large frames, with a worked example on household expenditure.
* The method comparison table (now Section 10) is recomputed with 2.0-2 and
  shows each solver's cost against the exact optimum.
* Removed a development-only block that sourced the R files from the source
  tree, and updated the package details at the top.

## Other

* Distribution fitting no longer prints optimiser errors and warnings to the
  console for families that fail to fit; those families are skipped quietly,
  as before.
* Added ORCID for the maintainer, and URL and BugReports fields.
* `DESCRIPTION` now describes the four solvers correctly (GLOBAL refines with
  BOBYQA, not COBYLA).
* `fit_distribution()` is documented as internal; it was marked for export
  but never exported.
* Comments in `cobyla.optim.R` updated to match the 2.0-2 behaviour.
* Help page for `strata.data()` corrected: `"dp"` is the default, and the
  `"exact"` method is documented.

---

# stratifyR 2.0-1  (2026-06-14)

## New features

* `strata.data()` and `strata.distr()` gain a `method` argument supporting
  three optimisation strategies:
  - `"dp"` (default) — the original exhaustive Dynamic Programming grid search,
    guaranteed globally optimal on the discrete grid.
  - `"cobyla"` — multi-start gradient-free local optimiser (COBYLA) via
    **nloptr**; faster than DP for large `h`, suitable for exploratory work.
  - `"global"` — two-phase global optimiser combining DIRECT-L (deterministic
    space-filling search) followed by COBYLA local refinement; closest to DP
    accuracy among the two new methods.
* New arguments `n_starts`, `max_iter`, `tol`, and `verbose` in both functions
  to control COBYLA/GLOBAL behaviour.
* `DESCRIPTION` updated: **nloptr** added to Imports.
* New `compare_designs()` function and print method. For the same total
  sample size it compares simple random sampling, proportional allocation and
  Neyman allocation, with design effects, the equivalent SRS sample size and
  the percentage cost saving.
* New `plot()` method for `"strata"` objects with three types: `"2d"`
  (histogram or density with boundaries and the fitted density formula),
  `"3d"` and `"interactive"` (both need **plotly**).
* New `stratifyRApp()` launches a self-contained **shiny** application:
  upload a CSV or Excel file or pick a built-in dataset, compute OSB and OSS
  with any solver, choose the number of strata, compare designs, explore the
  boundaries, download results and generate the equivalent R code.
* The COBYLA and GLOBAL solvers support the same cost-aware objective as DP
  (`cost = TRUE` with per-stratum costs `ch`).
* Solution-quality diagnostics for the COBYLA and GLOBAL solvers: a
  Cauchy-Schwarz optimality gap, KKT first-order residuals and a convergence
  flag, shown by `summary()`.

## Bug fixes

* **Weibull and Gamma recurrences (critical):** `distr.root()` and `data.root()`
  were using the *unregularized* upper incomplete gamma function `UGamma(s, z)`
  (via **zipfR**) where the *regularized* form `Q(s, z)` was required by the
  mathematical formulation. Fixed by replacing all calls with
  `pgamma(z, shape = s, lower.tail = FALSE)`, which correctly implements
  `Q(s, z) = Γ(s, z) / Γ(s)` as defined in Abramowitz & Stegun (1972),
  Chapter 6, equation 6.5.2. Results for Weibull and Gamma populations are now
  consistent with the published theory.
* **Triangular fitting in `.refit_real_scale()`:** simultaneous MLE of all three
  triangular parameters (min, max, mode) produced degenerate estimates
  (min ≈ max ≈ mode). Fixed by fixing min and max to sample bounds and
  estimating only the mode via MLE, matching the approach already used in
  `get.dist()`.
* **`get.dist()`:** triangular distribution now only selected over a tail model
  (Gamma, Weibull, Log-normal, Exponential) when its AIC advantage exceeds 10
  units, preventing spurious triangular wins on mildly right-skewed data.

## Vignette

* New Section 6: *Alternative Optimisation Methods: COBYLA and GLOBAL* —
  describes the motivation, algorithm, advantages, and limitations of each
  method, with a worked comparison example using the Anaemia/Iron data.
* New Section 8: *Summary of Method Comparison Across All Supported
  Distributions* — presents a table of DP vs COBYLA vs GLOBAL results across
  all ten distributions with explanation of observed patterns.
* Corrected description of `Q(r, y)` (regularized) vs `Γ(r, y)` (unregularized)
  in the Weibull and Gamma sections, with explicit R implementation note.
* Fixed incorrect Normal distribution example (wrong `mean`/`sd` parameters).
* Updated triangular parameter estimation example to use the fixed `fix.arg`
  approach.
* Minor fixes: removed duplicate `NeedsCompilation` field, removed stray
  `\url{}` in Introduction.

## Backward compatibility

* All `strata.data()` / `strata.distr()` call signatures from v1.0-5 work
  without modification (`method` defaults to `"dp"`).
* The `"strata"` S3 class and all slot names are unchanged.

---

# stratifyR 1.0-5  (2026-06-04)

Initial CRAN-submission-ready version. Implemented DP solver, 10-family
distribution fitting via AIC, Neyman allocation with cost support, and
coloured `summary.strata()` output.
