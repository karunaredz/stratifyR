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
#' @param my_env The environment my_env has various constants and calculations stored
#' from earlier opeartions through various other functions
#'
#' @return \code{} returns the array filled with calculations of objective
#' function values
#'
#' @author Karuna Reddy <karuna.reddy@auckland.ac.nz>\cr M
#' GM Khan <khan_mg@usp.ac.fj>
#'
# data.optim.R  (rewritten, safe bounds + comments)
data.optim <- function(k, n, incf, minYk, maxYk, isFirstRun = TRUE, my_env)
{
   # --- sanity checks for k ---
   if (length(k) > 1L || !is.numeric(k) || !is.finite(k) || k < 1L)
      stop("choice of 'k' is not valid")
   
   # --- grid step -> real distance for this column ---
   d <- n * incf
   
   # --- costs for current stratum k ---
   ch <- my_env$ch
   
   # --- argmin placeholders (value and y at min) ---
   dblRetVal <- Inf
   miny <- 0
   
   # --- special case: base stage (k == 1) ---
   #     Here the remaining width y must equal the remaining distance d.
   if (k == 1L) {
      y <- d
      cval <- ch[k]
      root <- data.root(d = d, y = y, c = cval, my_env = my_env)
      
      # invalid roots in your code return -1; keep that contract
      dblRetVal <- if (root != -1) root else Inf
      miny <- if (is.finite(dblRetVal)) y else 0
      
   } else {
      # --- clamp the search window on i (grid index for y) to valid range ---
      # i maps to y = i * incf, valid i is [0, n-1]; and caller may pass negatives.
      i_min <- max(0L, as.integer(minYk))
      i_max <- min(as.integer(maxYk) - 1L, as.integer(n) - 1L)
      
      # if no valid i in this window -> mark impossible and return
      if (i_min > i_max) {
         nr_m <- nrow(my_env$minkf2); nc_m <- ncol(my_env$minkf2)
         if (k + 1L <= nr_m && n + 1L <= nc_m) {
            my_env$minkf2[k + 1L, n + 1L] <- Inf
            my_env$dk2[  k + 1L, n + 1L] <- NA_real_
         }
         return(Inf)
      }
      
      # --- sweep candidate y (via i) within the safe window ---
      for (i in i_min:i_max) {
         y <- i * incf
         cval <- ch[k]
         
         # objective at current first stratum width y
         root <- data.root(d = d, y = y, c = cval, my_env = my_env)
         if (root == -1) next  # skip invalid branch early
         
         # remaining distance index (columns move "left" by i)
         col <- as.integer(n - i)
         
         # guard against any residual off-by-one / negative, AND matrix bounds
         nc_m <- ncol(my_env$minkf2)
         if (col < 0L || col > n || col + 1L > nc_m || col + 1L < 1L) next

         # --- DP memoization: if not computed, recurse; else read cache ---
         add <- my_env$minkf2[k, col + 1L]
         if (identical(add, -9999)) {
            # first pass has wide window; fine pass uses a narrow, centered window
            if (isTRUE(isFirstRun)) {
               add <- data.optim(k = k - 1L,
                                 n = col,
                                 incf = incf,
                                 minYk = 0L,
                                 maxYk = col,
                                 isFirstRun = TRUE,
                                 my_env = my_env)
            } else {
               add <- data.optim(k = k - 1L,
                                 n = col,
                                 incf = incf,
                                 minYk = my_env$ylimits[k] - my_env$factor * my_env$z,
                                 maxYk = my_env$ylimits[k] + my_env$factor * my_env$z,
                                 isFirstRun = FALSE,
                                 my_env = my_env)
            }
         }
         
         # combine current root with best of the remainder
         val <- root + add
         
         # argmin update
         if (val < dblRetVal) {
            dblRetVal <- val
            miny <- y
         }
      }
   }
   
   # --- write DP cell (store value and argmin y) ---
   # rows are k+1, columns are n+1 (1-based); guard against oversized indices
   nr_m <- nrow(my_env$minkf2); nc_m <- ncol(my_env$minkf2)
   if (k + 1L >= 1L && k + 1L <= nr_m && n + 1L >= 1L && n + 1L <= nc_m) {
      my_env$minkf2[k + 1L, n + 1L] <- dblRetVal
      my_env$dk2[  k + 1L, n + 1L] <- miny
   }
   
   return(dblRetVal)
}
##################################################################################
