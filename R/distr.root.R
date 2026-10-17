#' Calculate the objective function value for a given (d, y) under a
#' hypothesized distribution (scaled-data formulation)
#'
#' Used by the DP recurrences in the "distribution-known" pathway. All
#' distribution parameters are interpreted on the "scaled" axis
#' (initval .. initval+dist), consistent with 'strata.distr()'.
#'
#' @param d numeric: remaining distance/range on the (scaled) axis
#' @param y numeric: width of the first stratum on the (scaled) axis
#' @param c numeric: per stratum cost multiplier (Ch[k])
#' @param my_env environment: holds distribution name, scaled parameters,
#'   and scaling constants ('initval', 'maxval', etc.)
#'
#' @importFrom stats pgamma
#' @return numeric: sqrt(objective) or -1 if branch is infeasible/invalid
#'
#' @author Karuna Reddy <karuna.reddy@auckland.ac.nz>\cr
#' MGM Khan <khan_mg@usp.ac.fj>
distr.root <- function(d, y, c, my_env)
{
  ## ---------------------------------------------------------------------------
  ## Pull scaled constants from the environment
  ##  - initval: left edge on scaled axis (from strata.distr())
  ##  - maxval:  original data maximum (used to scale some params)
  ##  - distr:   hypothesized distribution name
  ## ---------------------------------------------------------------------------
  initval <- my_env$initval
  maxval  <- my_env$maxval
  distr   <- my_env$obj["distr"]
  
  ## Avoid shadowing the cost argument `c` in branches (e.g., triangle).
  cost <- c
  
  ## ---------------------------------------------------------------------------
  ## Helpers: replace zipfR::Cgamma / Rgamma with base-R equivalents
  ##  Gamma(s)                 = gamma(s)
  ##  Lambda(s, z) (upper)      = Lambda(s) * Q(s, z),  Q(s, z) = pgamma(z, s, lower.tail=FALSE)
  ## Using the **upper** incomplete gamma matches the original Rgamma usage.
  ## ---------------------------------------------------------------------------
  Gamma  <- function(s) base::gamma(s)
  UGamma <- function(s, z) Gamma(s) * pgamma(z, shape = s, lower.tail = FALSE)
  
  ## We'll accumulate the algebra into `calc` per branch and return sqrt(calc).
  calc <- NA_real_
  
  ## ===========================================================================
  ## Weibull branch (scaled)
  ##  Parameters on the scaled axis:
  ##   - r: shape (unchanged), t: scale / maxval, g: 0
  ##  Notes:
  ##   - g1r = Lambda(1 + 1/r), g2r = Lambda(1 + 2/r)
  ##   - Incomplete-gamma terms are differences of UGamma at transformed bounds.
  ## ===========================================================================
  if (distr == "weibull") {
    r <- my_env$obj[["params"]]["shape"]          # shape unchanged by scaling
    t <- my_env$obj[["params"]]["scale"] / maxval # scale is divided by maxval
    g <- 0
    
    g1r <- Gamma(1 + 1/r)
    g2r <- Gamma(1 + 2/r)

    ## Left and right boundary arguments (scaled)
    la <- ((d - y + initval - g) / t)^r
    ra <- ((d + initval - g) / t)^r

    ## A = t^2 * g2r * Wh  (unchanged - Wh uses exp(), no UGamma)
    A <- (t^2) * g2r * (exp(-la) - exp(-ra))

    ## B and C must use the REGULARISED upper incomplete gamma Q(s,z) =
    ## pgamma(z, shape=s, lower.tail=FALSE), NOT UGamma = Gamma(s)*Q(s,z).
    ## Using UGamma here inserts an extra Gamma(s) factor that distorts the
    ## objective so that sqrt(calc) != Wh*Sh.  With pgamma:
    ##   A*B  = t^2 * g2r * Wh * Q_diff_2  = Wh * integral(x^2 f(x))  [ok]
    ##   C^2  = t^2 * g1r^2 * Q_diff_1^2   = (integral(x f(x)))^2      [ok]
    ##   calc = Wh * E[X^2] - E[X]^2        = Wh^2 * Sh^2              [ok]
    B <- pgamma(la, shape = (2/r) + 1, lower.tail = FALSE) -
         pgamma(ra, shape = (2/r) + 1, lower.tail = FALSE)
    C <- t * g1r * (
      pgamma(la, shape = (1/r) + 1, lower.tail = FALSE) -
      pgamma(ra, shape = (1/r) + 1, lower.tail = FALSE)
    )

    calc <- (A * B - (C^2)) * cost
  }
  
  ## ===========================================================================
  ## Gamma branch (scaled)
  ##  Parameters on the scaled axis:
  ##   - r: shape (unchanged), f: rate * maxval, t = 1/f, g: 0
  ##  Notes:
  ##   - zipfR::Rgamma -> UGamma; prefactors unchanged.
  ## ===========================================================================
  if (distr == "gamma") {
    r <- my_env$obj[["params"]]["shape"]
    f <- my_env$obj[["params"]]["rate"] * maxval  # rate is multiplied by maxval
    t <- 1 / f                                    # scale on the scaled axis
    g <- 0
    
    ## Left/right scaled boundary args
    la <- (d - y + initval - g) / t
    ra <- (d + initval - g) / t

    ## Use regularised Q = pgamma(..., lower.tail=FALSE) instead of UGamma.
    ## Derivation (Gamma(r, scale=t)):
    ##   integral(x^k f(x)) = t^k * (Gamma(r+k)/Gamma(r)) * Q_diff_{r+k}
    ## With Q instead of UGamma the coefficients r*(r+1) and r carry the full
    ## Gamma ratio, giving:
    ##   A*B = t^2*r*(r+1)*Q_r * Q_{r+2}  = Wh * integral(x^2 f(x))   [ok]
    ##   C^2 = t^2*r^2 * Q_{r+1}^2        = (integral(x f(x)))^2       [ok]
    ##   calc = Wh^2 * Sh^2                                              [ok]
    A <- (t^2) * r * (r + 1) * (
      pgamma(la, shape = r,   lower.tail = FALSE) -
      pgamma(ra, shape = r,   lower.tail = FALSE)
    )
    B <- (
      pgamma(la, shape = r+2, lower.tail = FALSE) -
      pgamma(ra, shape = r+2, lower.tail = FALSE)
    )
    C <- t * r * (
      pgamma(la, shape = r+1, lower.tail = FALSE) -
      pgamma(ra, shape = r+1, lower.tail = FALSE)
    )

    calc <- (A * B - (C^2)) * cost
  }
  
  ## ===========================================================================
  ## Exponential branch (scaled)
  ##  lambda on the scaled axis is lambda * maxval.
  ## ===========================================================================
  if (distr == "exp") {
    lambda <- my_env$obj[["params"]]["rate"] * maxval
    
    A <- exp(-lambda * (d - y + initval))
    B <- (1/(lambda^2)) * (1 - exp(-lambda * y))^2
    C <- (y^2) * exp(-lambda * y)
    
    calc <- ((A^2) * (B - C)) * cost
  }
  
  ## ===========================================================================
  ## Normal branch (scaled)
  ##  mean and sd divide by maxval on the scaled axis.
  ## ===========================================================================
  if (distr == "norm") {
    mu    <- my_env$obj[["params"]]["mean"] / maxval
    sigma <- my_env$obj[["params"]]["sd"]   / maxval
    
    A <- erf((d + initval - mu)     / (sigma * sqrt(2))) -
      erf((d - y + initval - mu) / (sigma * sqrt(2)))
    B <- ((d - y + initval - mu) / sigma) *
      exp(-(((d - y + initval - mu) / (sigma * sqrt(2)))^2)) -
      ((d + initval - mu) / sigma) *
      exp(-(((d + initval - mu) / (sigma * sqrt(2)))^2))
    C <- exp(-(((d - y + initval - mu) / (sigma * sqrt(2)))^2)) -
      exp(-(((d + initval - mu) / (sigma * sqrt(2)))^2))
    
    calc <- (((sigma^2)/(2*sqrt(2*pi))) * A * B +
               0.25 * (sigma^2) * (A^2) -
               ((sigma^2)/(2*pi)) * (C^2)) * cost
  }
  
  ## ===========================================================================
  ## Lognormal branch (scaled)
  ##  meanlog shifts by -log(maxval); sdlog unchanged.
  ## ===========================================================================
  if (distr == "lnorm") {
    mu    <- my_env$obj[["params"]]["meanlog"] - log(maxval)
    sigma <- my_env$obj[["params"]]["sdlog"]
    
    A <- erf((log(d + initval)         - mu - 2*sigma^2) / (sigma * sqrt(2))) -
      erf((log(d - y + initval)     - mu - 2*sigma^2) / (sigma * sqrt(2)))
    B <- erf((log(d + initval)         - mu) / (sigma * sqrt(2))) -
      erf((log(d - y + initval)     - mu) / (sigma * sqrt(2)))
    C <- erf((log(d + initval)         - mu - sigma^2) / (sigma * sqrt(2))) -
      erf((log(d - y + initval)     - mu - sigma^2) / (sigma * sqrt(2)))
    
    calc <- (0.25 * exp(2*mu + 2*sigma^2) * A * B -
               0.25 * exp(2*mu +   sigma^2) * (C^2)) * cost
  }
  
  ## ===========================================================================
  ## Uniform branch (scaled)
  ##  min/max divide by maxval on the scaled axis.
  ## ===========================================================================
  if (distr == "unif") {
    minn <- my_env$obj[["params"]]["min"] / maxval
    maxx <- my_env$obj[["params"]]["max"] / maxval
    
    xh  <- d + initval
    xh1 <- d - y + initval
    
    wh    <- y / (maxx - minn)
    muh   <- (y + 2 * xh1) / 2
    sig2h <- (y^2) / 12
    
    calc <- (wh^2) * sig2h * cost
  }
  
  ## ===========================================================================
  ## Cauchy branch (scaled)
  ##  location and scale divide by maxval on the scaled axis.
  ## ===========================================================================
  if (distr == "cauchy") {
    l <- my_env$obj[["params"]]["location"] / maxval
    s <- my_env$obj[["params"]]["scale"]    / maxval
    
    xh  <- d + initval
    xh1 <- d - y + initval
    
    wh <- (1/pi) * (atan((xh1 + y - l)/s) - atan((xh1 - l)/s))
    
    muh <- (1/(2*pi*wh)) * (
      s * log((xh1 + y - l)^2 + s^2) + 2*l*atan((xh1 + y - l)/s)
      - s * log((xh1 - l)^2 + s^2)     - 2*l*atan((xh1 - l)/s)
    )
    
    sig2h <- (1/(pi*wh)) * (
      l*s*log((xh1 + y - l)^2 + s^2) + (l^2 - s^2)*atan((xh1 + y - l)/s)
      + s*(xh1 + y)
      - l*s*log((xh1 - l)^2 + s^2)     - (l^2 - s^2)*atan((xh1 - l)/s)
      - s*xh1
    ) - muh^2
    
    calc <- ((wh/pi) * (
      l*s*log((xh1 + y - l)^2 + s^2) + (l^2 - s^2)*atan((xh1 + y - l)/s)
      + s*(xh1 + y)
      - l*s*log((xh1 - l)^2 + s^2)     - (l^2 - s^2)*atan((xh1 - l)/s)
      - s*xh1
    ) -
      (1/(4*pi^2)) *
      ( s*log((xh1 + y - l)^2 + s^2) + 2*l*atan((xh1 + y - l)/s)
        - s*log((xh1 - l)^2 + s^2)     - 2*l*atan((xh1 - l)/s)
      )^2) * cost
  }
  
  ## ===========================================================================
  ## Triangle branch (scaled)
  ##  min/max/mode divide by maxval on the scaled axis.
  ##  (Guard against a==0 with a tiny epsilon when needed.)
  ## ===========================================================================
  if (distr == "triangle" || distr == "rtriangle") {
    a <- my_env$obj[["params"]]["min"] / maxval
    b <- my_env$obj[["params"]]["max"] / maxval
    m <- if (distr == "rtriangle") a else my_env$obj[["params"]]["mode"] / maxval
    xh1 <- d - y + initval
    # v2.0-3: exact moments over the stratum, splitting at the mode when the
    # stratum contains it (the old formula used one side of the triangle only)
    calc <- .triang_w2s2(xh1, xh1 + y, a, b, m) * cost
  }
  
  ## ===========================================================================
  ## Pareto type II branch (scaled)
  ##  scale divides by maxval on the scaled axis; shape unchanged.
  ## ===========================================================================
  if (distr == "pareto") {
    a <- my_env$obj[["params"]]["shape"]
    s <- my_env$obj[["params"]]["scale"] / maxval
    
    xh  <- d + initval
    xh1 <- d - y + initval
    
    # v2.0-3: numerically stable form, valid for every shape > 0
    # (the old closed form divided by (2 - a) and (1 - a)).
    calc <- .pareto_w2s2(xh1, y, a, s) * cost
  }
  
  ## ---------------------------------------------------------------------------
  ## Return as per contract:
  ##  - If calc is non-finite or < 0 -> infeasible branch => -1
  ##  - Otherwise return sqrt(calc)
  ## ---------------------------------------------------------------------------
  if (!is.finite(calc) || calc < 0) {
    return(-1)
  } else {
    return(sqrt(calc))
  }
}
######################################################
