#' To create and store calculated values of the objective function
#'
#' This function creates a matrix whose rows and columns depend on the
#' range or distance of the data and the number of strata solutions that the
#' user is seeking to compute. The matrix stores the objective function values
#' calculated by the algorithm only to be accessed later for the purpose of
#' presenting the OSB.
#'
#' @param my_env The environment my_env has various constants stored
#' from earlier operations dealing with information on the data
#'
#' @return \code{} stores numerical quantities of the objective function
#' and stores in the two matrices inside the my_env to be accessed by other
#' functions
#'
#' @author Karuna Reddy <karuna.reddy@auckland.ac.nz>\cr
#' MGM Khan <khan_mg@usp.ac.fj>
#'
create.mat <- function(my_env) {
  # pull sizes
  stages <- as.integer(my_env$stages)
  e <- as.integer(my_env$e)
  
  message("The program is running, it'll take some time!")
  
  # basic sanity checks (defensive)
  if (!is.finite(stages) || stages < 2L)
    stop("create.mat(): 'stages' must be an integer >= 2; got ", stages)
  if (!is.finite(e) || e < 1L)
    stop("create.mat(): 'e' must be an integer >= 1; got ", e)
  
  # DP tables:
  # minkf2 holds best objective values (non-negative), so -9999 is a safe sentinel.
  my_env$minkf2 <- matrix(-9999.0, nrow = stages, ncol = e)
  
  # dk2 holds argmins (y values / indices). NA is a clearer sentinel than -9999.
  my_env$dk2    <- matrix(NA_real_, nrow = stages, ncol = e)
  
  invisible(NULL)
}
########################################################################
