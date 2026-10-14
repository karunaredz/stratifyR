#' To implement the Dynamic Programming (DP) solution procedure on the stratification
#' problem presented in the form of a Mathematical Programming Problem (MPP)
#'
#' This function uses the Dynamic Programming (DP) solution procedure in solving the
#' objective function for the univariate stratification problem. It calculates
#' the objective function values using the brute-force algorithm and stores those
#' values in the matrices and keeps a copy in my_env so that a global minimum
#' could be obtained.
#'
#' @param k A numeric: number of strata
#' @param n A numeric: is the distance*1000
#' @param incf A numeric: 10e-3 when k=1 and 10e-5 for k>=2
#' @param minYk A numeric: index to access minimum elements in the matrix
#' @param maxYk A numeric: index to access maximum elements in the matrix
#' @param isFirstRun A boolean: TRUE/FALSE parameter
#' @param my_env My environment my_env has various constants and calculations stored
#' from earlier opeartions through various other functions
#'
#' @return \code{} returns the array filled with calculations of objective
#' function values
#'
#' @author Karuna Reddy <karuna.reddy@auckland.ac.nz>\cr MGM Khan <khan_mg@usp.ac.fj>
#'
# distr.optim.R  (rewritten: safe bounds, window clamp, guarded indexing)
distr.optim <- function(k, n, incf, minYk, maxYk, isFirstRun = TRUE, my_env)
{
   # --- sanity checks for k ---
   if (length(k) > 1L || !is.numeric(k) || !is.finite(k) || k < 1L)
      stop("choice of 'k' is not valid")
   
   # --- grid step -> real remaining distance for this DP cell ---
   d <- n * incf
   
   # --- costs vector (per stratum) ---
   ch <- my_env$ch
   
   # --- argmin placeholders (value and y at min) ---
   dblRetVal <- Inf
   miny <- 0
   
   # --- base case: last stratum (k == 1) ---
   #     must consume entire remaining distance d as width y
   if (k == 1L) {
      y <- d
      cval <- ch[k]
      root <- distr.root(d = d, y = y, c = cval, my_env = my_env)
      
      # keep your invalid-root contract (-1 means unusable)
      dblRetVal <- if (root != -1) root else Inf
      miny <- if (is.finite(dblRetVal)) y else 0
      
   } else {
      # --- clamp i (index for y = i * incf) to safe window ---
      # valid grid i is [0, n-1]; caller may pass negatives/wide windows
      i_min <- max(0L, as.integer(minYk))
      i_max <- min(as.integer(maxYk) - 1L, as.integer(n) - 1L)
      
      # no feasible i in this window -> mark as impossible and return
      if (i_min > i_max) {
         nr_m <- nrow(my_env$minkf2); nc_m <- ncol(my_env$minkf2)
         if (k + 1L <= nr_m && n + 1L <= nc_m) {
            my_env$minkf2[k + 1L, n + 1L] <- Inf
            my_env$dk2[  k + 1L, n + 1L] <- NA_real_
         }
         return(Inf)
      }
      
      # --- sweep candidate first-stratum widths y over [i_min, i_max] ---
      for (i in i_min:i_max) {
         y <- i * incf
         cval <- ch[k]
         
         # objective for the first stratum of width y
         root <- distr.root(d = d, y = y, c = cval, my_env = my_env)
         if (root == -1) next  # skip invalid branch early
         
         # remaining distance index (in columns) after taking i steps
         col <- as.integer(n - i)
         
         # guard against residual off-by-one / negative, AND matrix bounds
         nc_m <- ncol(my_env$minkf2)
         if (col < 0L || col > n || col + 1L > nc_m || col + 1L < 1L) next

         # --- DP memoization: compute subproblem if needed, else reuse cache ---
         add <- my_env$minkf2[k, col + 1L]
         if (identical(add, -9999)) {
            if (isTRUE(isFirstRun)) {
               add <- distr.optim(k = k - 1L,
                                  n = col,
                                  incf = incf,
                                  minYk = 0L,
                                  maxYk = col,
                                  isFirstRun = TRUE,
                                  my_env = my_env)
            } else {
               add <- distr.optim(k = k - 1L,
                                  n = col,
                                  incf = incf,
                                  minYk = my_env$ylimits[k] - my_env$factor * my_env$z,
                                  maxYk = my_env$ylimits[k] + my_env$factor * my_env$z,
                                  isFirstRun = FALSE,
                                  my_env = my_env)
            }
         }
         
         # combine current stratum cost with best remainder
         val <- root + add
         
         # argmin update
         if (val < dblRetVal) {
            dblRetVal <- val
            miny <- y
         }
      }
   }
   
   # --- write DP cell (store value and argmin y) ---
   # DP tables are 1-based: rows = k+1, cols = n+1; guard against oversized indices
   nr_m <- nrow(my_env$minkf2); nc_m <- ncol(my_env$minkf2)
   if (k + 1L >= 1L && k + 1L <= nr_m && n + 1L >= 1L && n + 1L <= nc_m) {
      my_env$minkf2[k + 1L, n + 1L] <- dblRetVal
      my_env$dk2[  k + 1L, n + 1L] <- miny
   }
   
   return(dblRetVal)
}
##################################################################################
