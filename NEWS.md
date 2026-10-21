# stratifyR (development version)

## Shiny app

* Drop-down lists (built-in datasets and data columns) now have a solid
  background, so every option is readable, not only the first one. Long
  lists scroll, and the option under the mouse is highlighted.
* Long dashes in the app's labels and text were replaced with plain hyphens.

---

# stratifyR 2.0-3  (2026-10-16)

A robustness release for very heavy-tailed populations, found with a 20-seed
repeat of the simulation study.

## Solvers

* `method = "cobyla"` on data: one of the starting points now comes from a
  quick, coarse exact search (dynamic programming over about 400 quantile cut
  points, a few milliseconds). On infinite-variance samples such as a Pareto
  with shape 2, whose range can be hundreds of times the interquartile range,
  COBYLA previously missed the narrow lower strata at larger `h` (up to 170%
  above the optimum); it is now within 1% of the exact optimum in all our tests.
* `method = "cobyla"`: random restarts now perturb the starting probabilities
  rather than the boundary values, and the minimum separation between
  boundaries is a tiny fraction of the range instead of `range/1000`.
* `method = "global"` on data now searches on the rank scale (boundaries are
  parameterised by their position in the sorted data), so DIRECT-L covers the
  lower tail as finely as the rest of the data.
* A failed COBYLA solve on extreme data could stop `strata.data()` with an
  obscure `findInterval()` error; it is now handled.
* When the DP recursion cannot find valid boundaries for the fitted
  distribution (possible on very heavy-tailed data), `strata.data()` now stops
  with a clear message suggesting `method = "exact"` or `method = "cobyla"`.

* `method = "dp"`: after the DP search on the fitted model, a short local
  refinement (BOBYQA) of the boundaries is tried. It is kept only when it
  lowers the model cost and gives valid strata, so it can never make the
  DP solution worse. This removes most of the error caused by the grid.
* `method = "dp"`: two fixes in the original DP traceback. Grid positions
  were converted to table indices by truncation, so a value such as
  3.9999999 grid steps was read as step 3; they are now rounded. And the
  second, finer pass only fills a window of the table around the first
  answer; when its answer moved to the edge of that window, earlier stages
  read empty cells and the solver stopped. It now keeps the first-pass
  answer in that case (the final refinement improves it anyway).
* `method = "exact"`: when very large data are thinned to a candidate set,
  the smallest and largest values are now always kept.

## Distribution fitting

* Pareto and triangular fits never succeeded inside the package (the
  fitting function looked for `dpareto()` and `dtriang()` on the search
  path), so these families were never chosen and DP used a wrong model on
  such data. Both are now fitted by maximum likelihood with their own code:
  the Pareto (Lomax) shape and scale by Nelder-Mead and BFGS on the log
  scale, and the triangle by profiling the likelihood over the mode with
  the bounds set just outside the data range.
* The Pareto log-likelihood is computed in a numerically stable form. The
  textbook form subtracts two huge, nearly equal numbers when the shape and
  scale are large; on Apple silicon the optimiser could drift there, the
  log-likelihood collapsed to about zero, and the Pareto then wrongly won
  the AIC comparison on gamma, Weibull, uniform and triangular data (and
  the DP then stopped).
* The Cauchy fit now uses robust starting values (median and half the IQR)
  and keeps the scale positive, so it no longer fails on extreme samples.
* `strata.data()` reports right-triangular data as `"rtriangle"` correctly,
  and fitted parameters are moved back to the data scale correctly for the
  Pareto and triangular families.

## Stratum variances

* Pareto strata: the truncated moments are computed in a numerically stable
  form, which matters for shapes near 2 and wide strata.
* Triangular and right-triangular strata: exact closed-form truncated
  moments (split at the mode) replace the previous approximation.
* Both were checked against numerical integration to about 1e-14.

## Other fixes

* `method = "cobyla"` and `method = "global"` on data: the objective rebuilt
  the outer limits of the range as `initval * maxval` and `finval * maxval`,
  which can round a hair inside the data, so the smallest or largest unit
  was silently left out while optimising. On data with extreme values in
  both tails (for example a Cauchy sample) the solvers then optimised the
  wrong cost and could end up far from the optimum (up to 41% in a 20-seed
  study). Units are now assigned with the interior boundaries only, exactly
  as in the final allocation, and both solvers reach the exact optimum on
  these samples. The same fix is applied in the interactive boundary plot.
* `method = "cobyla"` on data now keeps the coarse exact start as a
  candidate answer, so its result can never be worse than that start, and
  the coarse search keeps more cut points in both tails.

* `method = "cobyla"` and `method = "global"` on data now place each boundary
  halfway between the two data values on either side of it. Before, on data
  with many tied values, rounding could move the tied units at a boundary
  into the next stratum, so the reported `WhShTot` could be slightly higher
  than the cost the solver had found (0.2% on the `math` data at `h = 3`).
* `strata.distr()` now stops with a clear message when the range
  `[initval, initval + dist]` lies outside the support of a uniform or
  triangular distribution, instead of failing inside the allocation step.
* `strata.distr()`: the stratum population sizes `Nh` now add up to `N`
  exactly (largest remainder). Before, each `N * Wh` was rounded on its own,
  so the total could be off by one.
* `summary()` output inside R Markdown and Quarto documents now renders as
  formatted HTML. Before, the heading lines appeared as raw HTML text.

## Documentation

* The vignette describes the new solver details, its comparison table was
  recomputed with this version, and the right-triangular and uniform examples
  were corrected to match what the package now fits.

## Tests

* New unit tests: COBYLA within 2% of the exact optimum on an
  infinite-variance Pareto sample at `h = 4`; COBYLA and GLOBAL equal to the
  exact optimum on tied data; the new range check in `strata.distr()`;
  COBYLA equal to the exact optimum on a two-sided heavy-tailed sample.

---

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
