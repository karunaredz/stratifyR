#' Calculate the objective function value for a given (d, y)
#'
#' Used by the DP recurrences to evaluate the stratification objective at
#' a specific remaining distance 'd' and first-stratum width 'y', given
#' stratum cost 'c' and constants tucked in 'my_env.'
#'
#' @param d numeric: remaining distance/range on the (scaled) axis
#' @param y numeric: first stratum width on the (scaled) axis
#' @param c numeric: per stratum cost multiplier (Ch[k])
#' @param my_env environment: holds distribution name, parameters and scaling
#'
#' @importFrom stats pgamma
#' @return numeric: sqrt(objective) or -1 if branch is infeasible/invalid
data.root <- function(d, y, c, my_env)
{
  ## ---------------------------------------------------------------------------
  ## Pull inputs from the environment
  ##  - initval: left edge (scaled units)
  ##  - ch:      vector of stratum costs (not used directly here; 'c' is used)
  ##  - distr:   best-fit distribution name
  ##  - params:  distribution parameters (accessed below per-branch)
  ## ---------------------------------------------------------------------------
  initval <- my_env$initval
  ch      <- my_env$ch
  distr   <- my_env$obj["distr"]
  
  ## ---------------------------------------------------------------------------
  ## Helpers: replace zipfR::Cgamma / Rgamma with base-R equivalents
  ##  Gamma(s)                 = gamma(s)
  ##  Lambda(s, z) (upper)      = Lambda(s) * Q(s, z),  Q(s, z) = pgamma(z, s, lower.tail=FALSE)
  ## Using the UPPER incomplete gamma matches your original 'Rgamma' usage.
  ## ---------------------------------------------------------------------------
  Gamma  <- function(s) base::gamma(s)
  UGamma <- function(s, z) Gamma(s) * pgamma(z, shape = s, lower.tail = FALSE)
  
  ## We'll fill 'calc' in each distribution branch and then return sqrt(calc).
  calc <- NA_real_
  
  ## ===========================================================================
  ## Weibull branch
  ##  Parameters:
  ##   - r: shape, t: scale, g: location (fixed 0 here)
  ##  Notes:
  ##   - g1r = Lambda(1 + 1/r), g2r = Lambda(1 + 2/r)
  ##   - Incomplete gamma terms become differences of UGamma at transformed bounds.
  ## ===========================================================================
  if (distr == "weibull") {
    r <- my_env$obj[["params"]]["shape"]
    t <- my_env$obj[["params"]]["scale"]
    g <- 0
    
    g1r <- Gamma(1 + 1/r)
    g2r <- Gamma(1 + 2/r)

    la <- ((d - y + initval - g) / t)^r
    ra <- ((d + initval - g) / t)^r

    ## Use regularised Q = pgamma(..., lower.tail=FALSE) - NOT UGamma which
    ## includes an extra Gamma(s) factor that distorts calc away from Wh^2*Sh^2.
    A <- (t^2) * g2r * (exp(-la) - exp(-ra))
    B <- pgamma(la, shape = (2/r) + 1, lower.tail = FALSE) -
         pgamma(ra, shape = (2/r) + 1, lower.tail = FALSE)
    C <- t * g1r * (
      pgamma(la, shape = (1/r) + 1, lower.tail = FALSE) -
      pgamma(ra, shape = (1/r) + 1, lower.tail = FALSE)
    )

    calc <- (A * B - (C^2)) * c
  }

  ## ===========================================================================
  ## Gamma branch
  ##  Parameters:
  ##   - r: shape, f: rate, t = 1/f: scale, g: location (0)
  ## ===========================================================================
  if (distr == "gamma") {
    r <- my_env$obj[["params"]]["shape"]
    f <- my_env$obj[["params"]]["rate"]
    t <- 1 / f
    g <- 0

    la <- (d - y + initval - g) / t
    ra <- (d + initval - g) / t

    ## pgamma replaces UGamma for the same reason as the Weibull branch above.
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

    calc <- (A * B - (C^2)) * c
  }
  
  ## ===========================================================================
  ## Exponential branch
  ##  Uses closed-form pieces (no special functions needed).
  ## ===========================================================================
  if (distr == "exp") {
    lambda <- my_env$obj[["params"]]["rate"]
    
    A <- exp(-lambda * (d - y + initval))
    B <- (1/(lambda^2)) * (1 - exp(-lambda * y))^2
    C <- (y^2) * exp(-lambda * y)
    
    calc <- ((A^2) * (B - C)) * c
  }
  
  ## ===========================================================================
  ## Normal branch
  ##  Uses error-function helpers (you already defined erf()).
  ## ===========================================================================
  if (distr == "norm") {
    mu    <- my_env$obj[["params"]]["mean"]
    sigma <- my_env$obj[["params"]]["sd"]
    
    A <- erf((d + initval - mu)     / (sigma * sqrt(2))) -
      erf((d - y + initval - mu) / (sigma * sqrt(2)))
    B <- ((d - y + initval - mu) / sigma) *
      exp(-(((d - y + initval - mu) / (sigma * sqrt(2)))^2)) -
      ((d + initval - mu) / sigma) *
      exp(-(((d + initval - mu) / (sigma * sqrt(2)))^2))
    C <- exp(-(((d - y + initval - mu) / (sigma * sqrt(2)))^2)) -
      exp(-(((d + initval - mu) / (sigma * sqrt(2)))^2))
    
    calc <- (((sigma^2) / (2 * sqrt(2*pi))) * A * B +
               0.25 * (sigma^2) * (A^2) -
               ((sigma^2) / (2*pi)) * (C^2)) * c
  }
  
  ## ===========================================================================
  ## Lognormal branch
  ##  Re-uses your existing algebra in terms of erf on log-transformed bounds.
  ## ===========================================================================
  if (distr == "lnorm") {
    mu    <- my_env$obj[["params"]]["meanlog"]
    sigma <- my_env$obj[["params"]]["sdlog"]
    
    A <- erf((log(d + initval)         - mu - 2*sigma^2) / (sigma * sqrt(2))) -
      erf((log(d - y + initval)     - mu - 2*sigma^2) / (sigma * sqrt(2)))
    B <- erf((log(d + initval)         - mu) / (sigma * sqrt(2))) -
      erf((log(d - y + initval)     - mu) / (sigma * sqrt(2)))
    C <- erf((log(d + initval)         - mu - sigma^2) / (sigma * sqrt(2))) -
      erf((log(d - y + initval)     - mu - sigma^2) / (sigma * sqrt(2)))
    
    calc <- (0.25 * exp(2*mu + 2*sigma^2) * A * B -
               0.25 * exp(2*mu +   sigma^2) * (C^2)) * c
  }
  
  ## ===========================================================================
  ## Cauchy branch
  ##  Closed forms for truncated Cauchy; you had these already.
  ## ===========================================================================
  if (distr == "cauchy") {
    mu  <- my_env$obj[["params"]]["location"]
    sig <- my_env$obj[["params"]]["scale"]
    
    xh  <- d + initval
    xh1 <- d - y + initval
    
    wh <- (1/pi) * (atan((xh1 + y - mu)/sig) - atan((xh1 - mu)/sig))
    
    muh <- (1 / (2 * (atan((xh1 + y - mu)/sig) - atan((xh1 - mu)/sig)))) *
      ( sig * log((xh1 + y - mu)^2 + sig^2) + 2*mu*atan((xh1 + y - mu)/sig)
        -sig * log((xh1 - mu)^2 + sig^2)     - 2*mu*atan((xh1 - mu)/sig) )
    
    sig2h <- (1 / (atan((xh1 + y - mu)/sig) - atan((xh1 - mu)/sig))) *
      ( mu*sig*log((xh1 + y - mu)^2 + sig^2)
        + (mu^2 - sig^2)*atan((xh1 + y - mu)/sig)
        + sig*(xh1 + y)
        - mu*sig*log((xh1 - mu)^2 + sig^2)
        - (mu^2 - sig^2)*atan((xh1 - mu)/sig)
        - sig*xh1) - muh^2
    
    calc <- ((1/pi^2) * (atan((xh1 + y - mu)/sig) - atan((xh1 - mu)/sig)) *
               ( mu*sig*log((xh1 + y - mu)^2 + sig^2)
                 + (mu^2 - sig^2)*atan((xh1 + y - mu)/sig)
                 + sig*(xh1 + y)
                 - mu*sig*log((xh1 - mu)^2 + sig^2)
                 - (mu^2 - sig^2)*atan((xh1 - mu)/sig)
                 - sig*xh1) -
               (1/(4*pi^2)) *
               ( sig*log((xh1 + y - mu)^2 + sig^2)
                 + 2*mu*atan((xh1 + y - mu)/sig)
                 - sig*log((xh1 - mu)^2 + sig^2)
                 - 2*mu*atan((xh1 - mu)/sig) )^2) * c
  }
  
  ## ===========================================================================
  ## Triangle branch
  ##  Piecewise based on mode; matches your original formulas.
  ## ===========================================================================
  if (distr == "triangle" || distr == "rtriangle") {
    a <- my_env$obj[["params"]]["min"]
    b <- my_env$obj[["params"]]["max"]
    m <- if (distr == "rtriangle") a else my_env$obj[["params"]]["mode"]
    xh1  <- d - y + initval
    # v2.0-3: exact moments over the stratum, splitting at the mode when the
    # stratum contains it (the old formula used one side of the triangle only)
    calc <- .triang_w2s2(xh1, xh1 + y, a, b, m) * c
  }
  
  ## ===========================================================================
  ## Uniform branch
  ## ===========================================================================
  if (distr == "unif") {
    minn <- my_env$obj[["params"]]["min"]
    maxx <- my_env$obj[["params"]]["max"]
    
    xh  <- d + initval
    xh1 <- d - y + initval
    
    wh    <- y / (maxx - minn)
    muh   <- (y + 2 * xh1) / 2
    sig2h <- (y^2) / 12
    
    calc <- (wh^2) * sig2h * c
  }
  
  ## ===========================================================================
  ## Pareto type II branch
  ##  Uses your existing closed-form expressions.
  ## ===========================================================================
  if (distr == "pareto") {
    a <- my_env$obj[["params"]]["shape"]
    s <- my_env$obj[["params"]]["scale"]
    
    xh  <- d + initval
    xh1 <- d - y + initval
    
    # v2.0-3: numerically stable form, valid for every shape > 0
    # (the old closed form divided by (2 - a) and (1 - a)).
    calc <- .pareto_w2s2(xh1, y, a, s) * c
  }
  
  ## ---------------------------------------------------------------------------
  ## Convert to objective value:
  ##  - Negative/NaN/NA -> infeasible branch => return -1 (as in your contract)
  ##  - Otherwise return sqrt(calc) (objective uses sqrt of the computed form)
  ## ---------------------------------------------------------------------------
  if (!is.finite(calc) || calc < 0) {
    return(-1)
  } else {
    return(sqrt(calc))
  }
}
#########################################################
# -----------------------------------------------------------------------------
# .pareto_w2s2 (internal, v2.0-3)
# -----------------------------------------------------------------------------
# W_h^2 S_h^2 for the stratum [L, L + y] of a Pareto type II (Lomax) variable
# with shape a and scale s, i.e. M0 * M2 - M1^2 where Mk are the truncated
# moments of T = X + s. Each moment is an integral of t^(e - 1) between
# T1 = L + s and T2 = L + y + s, computed as T1^e * expm1(e log(T2/T1)) / e,
# which stays accurate as e -> 0. This removes the removable singularities of
# the textbook closed form at a = 1 and a = 2 (heavy tails with infinite
# variance have fitted shapes close to 2).
.pareto_w2s2 <- function(L, y, a, s) {
  T1 <- L + s
  T2 <- L + y + s
  if (!is.finite(T1) || !is.finite(T2) || T1 <= 0 || T2 <= T1) return(NaN)
  lr <- log(T2 / T1)
  g  <- function(e) if (abs(e * lr) < 1e-12) lr * T1^e else T1^e * expm1(e * lr) / e
  k  <- a * s^a
  M0 <- k * g(-a); M1 <- k * g(1 - a); M2 <- k * g(2 - a)
  as.numeric(M0 * M2 - M1^2)
}

# -----------------------------------------------------------------------------
# .triang_w2s2 (internal, v2.0-3)
# -----------------------------------------------------------------------------
# W_h^2 S_h^2 = M0 * M2 - M1^2 for the stratum [L, U] of a triangular
# distribution with min a, mode m and max b, where Mk are the exact truncated
# moments. The density is linear on each side of the mode, so each moment is a
# polynomial integral; the stratum is split at the mode when it contains it.
# Values are shifted by a for accuracy (the variance is shift invariant).
.triang_w2s2 <- function(L, U, a, b, m) {
  L <- max(L, a); U <- min(U, b)
  if (!is.finite(L) || !is.finite(U) || !(U > L) || !(b > a)) return(NaN)
  B <- b - a; M <- min(max(m - a, 0), B); l <- L - a; u <- U - a
  add <- function(lo, hi, c0, c1) {
    if (hi <= lo) return(c(0, 0, 0))
    vapply(0:2, function(k) c0 * (hi^(k + 1) - lo^(k + 1)) / (k + 1) +
                            c1 * (hi^(k + 2) - lo^(k + 2)) / (k + 2), 0)
  }
  mom <- c(0, 0, 0)
  if (M > 0) mom <- mom + add(l, min(u, M), 0, 2 / (B * M))
  if (M < B) mom <- mom + add(max(l, M), u, 2 / (B - M), -2 / (B * (B - M)))
  as.numeric(mom[1] * mom[3] - mom[2]^2)
}
