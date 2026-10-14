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
  if (distr == "triangle") {
    a  <- my_env$obj[["params"]]["min"]
    b  <- my_env$obj[["params"]]["max"]
    m  <- my_env$obj[["params"]]["mode"]
    if (initval == 0) initval <- initval + 1e-10
    
    xh  <- d + initval
    xh1 <- d - y + initval
    
    # Left piece (up-slope)
    wh1    <- (y * (y + 2 * (xh1 - a))) / ((b - a) * (m - a))
    muh1   <- ((2/3) * y^2 + 2*y*xh1 - a*y + 2*(xh1 - a)*xh1) / (y + 2*(xh1 - a))
    sig2h1 <- (y^2 * (y^2 + 6*y*(xh1 - a) + 6*(xh1 - a)^2)) / (18 * (y + 2*(xh1 - a))^2)
    
    # Right piece (down-slope)
    wh2    <- (y * (2*(b - xh1) - y)) / ((b - a) * (b - m))
    muh2   <- (3*(b - xh1)*y - 3*y*xh1 + 6*(b - xh1)*xh1 - 2*y^2) / (3 * (2*(b - xh1) - y))
    sig2h2 <- (y^2 * (6*(b - xh1)^2 - 6*y*(b - xh1) + y^2)) / (18 * (2*(b - xh1) - y)^2)
    
    calc <- if (d <= m) (wh1^2) * sig2h1 * c else (wh2^2) * sig2h2 * c
  }
  
  ## ===========================================================================
  ## Right-triangle branch
  ## ===========================================================================
  if (distr == "rtriangle") {
    a <- my_env$obj[["params"]]["min"]
    b <- my_env$obj[["params"]]["max"]
    if (initval == 0) initval <- initval + 1e-10
    
    xh  <- d + initval
    xh1 <- d - y + initval
    
    wh    <- (y * (2*(b - xh1) - y)) / ((b - a)^2)
    muh   <- (3*b*(y + 2*xh1) - 2*(y^2 + 3*y*xh1) + 3*xh1^2) / (3 * (2*(b - xh1) - y))
    sig2h <- (y^2 * (y^2 - 6*(b - xh1)*y + 6*(b - xh1)^2)) / (18 * (2*(b - xh1) - y)^2)
    
    calc <- (wh^2) * sig2h * c
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
    
    Wh <- (s / (xh1 + s))^a - (s / (y + xh1 + s))^a
    A  <- ( (y + xh1 + s)^(2 - a) ) / (2 - a)
    B  <- ( 2 * s * (y + xh1 + s)^(1 - a) ) / (1 - a)
    C0 <- ( (s^2) * (y + xh1 + s)^(-a) ) / a
    D  <- ( (xh1 + s)^(2 - a) ) / (2 - a)
    E  <- ( 2 * s * (xh1 + s)^(1 - a) ) / (1 - a)
    G  <- ( (s^2) * (xh1 + s)^(-a) ) / a
    H  <- (a * (y + xh1) + s) / ((y + xh1 + s)^a)
    I  <- (a * xh1 + s) / ((xh1 + s)^a)
    
    calc <- ((a * s^a) * Wh * (A - B - C0 - D + E + G) -
               ((s^(2*a)) / (1 - a)^2) * (H - I)^2) * c
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