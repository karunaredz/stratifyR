#' To re-allocate the stratum sample sizes (nh)
#'
#' This function re-calculates or re-allocate the stratum sample sizes 
#' (nh) after it has already been initially allocated via Neyman
#' allocation. This is applied to resolve the problem of oversampling 
#' in one or more of the strata.
#'
#' @param h A numeric: the no. of strata
#' @param x A vector: the osb that has been calculated
#' @param nh A vector: the stratum sample sizes that have been initially calculated
#' @param Nh A vector: the stratum population sizes that have been initially calculated
#' @param nume A numeric: the numerator total
#' @param my_env The environment my_env has various constants and outputs stored
#' from earlier opeartions through various other functions
#'
#' @return \code{} calculates and presents the new re-allocate stratum samples
#'
#' @author Karuna Reddy <karuna.reddy@auckland.ac.nz>\cr
#' MGM Khan <khan_mg@usp.ac.fj>
#'
realloc <- function(h, x, nh, Nh, nume, my_env){

Dh <- double(h)
deno.new <- 0
DhTot <- 0

for(i in 1:(length(x)-1))
   {
      # Guard against NA/NaN nh values (can arise from empty strata)
      if(is.na(nh[i]) || is.nan(nh[i])) nh[i] <- 0
      if(is.na(Nh[i])) Nh[i] <- 0

      #nh is less than or equal to stratum population totals
      if(nh[i] == Nh[i]){
         nh[i] <- nh[i] #no problem
      }
      #oversampling case, where nh is more than what's available in Nh
      else if(nh[i] > Nh[i]){
         Dh[i] <- nh[i]-Nh[i]# extra units, calculate difference first
         DhTot <- DhTot + Dh[i]
         nh[i] <- Nh[i] # assign all units in pop stratum to the sample stratum
      }
      else { #for the ok case: nh < Nh
         deno.new <- deno.new + nume[i]
      }
   }

   #re-alloc as per weighting of WhSh where pop units are available
   for(i in 1:(length(x)-1))
   {
      if(!is.na(nh[i]) && !is.na(Nh[i]) && nh[i] < Nh[i]){
         if(deno.new > 0) {
            nh[i] <- nh[i] + DhTot*(nume[i]/deno.new)#add to strata where space available
         }
      }
   }
  my_env$nh <- nh #return samples
}
######################################################################################S
# -----------------------------------------------------------------------------
# .round_alloc (internal, v2.0-2)
#
# Rounds the stratum sample sizes to whole numbers so that they add up to the
# requested total and no stratum takes more units than it has. Rounding each
# n_h on its own can make the total one more or one less than n (for example
# 62.5 + 102.5 + 135 -> 63 + 103 + 135 = 301). This uses the largest-remainder
# rule: round every n_h down, then give the leftover units, one each, to the
# strata with the largest fractional parts that still have room.
# -----------------------------------------------------------------------------
.round_alloc <- function(nh, Nh) {
  nh[!is.finite(nh)] <- 0
  cap    <- floor(Nh + 1e-8)
  target <- min(round(sum(nh)), sum(cap))
  base   <- pmin(floor(nh + 1e-8), cap)
  left   <- target - sum(base)
  if (left > 0) {
    frac <- nh - base
    frac[base >= cap] <- -Inf
    ord  <- order(frac, decreasing = TRUE)
    for (i in ord) {
      if (left <= 0) break
      if (base[i] < cap[i]) { base[i] <- base[i] + 1; left <- left - 1 }
    }
  }
  base
}
