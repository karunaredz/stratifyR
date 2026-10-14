#' To calculate the stratum sample sizes (nh) for a fixed sample size (n) based
#' on the hypothetical distribution of the data
#'
#' Uses OSB to compute stratum weights (Wh), variances (Vh), Neyman allocations (nh),
#' and related totals under a *given* population distribution. Integrations are
#' cached per stratum; the output data.frame is built once at the end (faster).
#'
#' @param my_env Environment carrying all precomputed values and constants.
#' @return Populates my_env$output, my_env$out, and totals (WhTot, NhTot, etc.)
distr.alloc <- function(my_env)
{
  # ----------------------- unpack common inputs ------------------------------
  h       <- my_env$h
  initval <- my_env$initval * my_env$maxval               # back to real (unscaled) units
  x       <- c(initval, my_env$df$x * my_env$maxval)      # OSB on real scale (including left edge)
  n       <- my_env$n
  N       <- my_env$N
  ch      <- my_env$ch                                     # per-stratum unit cost (length h)
  distr   <- my_env$obj["distr"]                           # distribution name (character)
  
  # ----------------------- preallocate outputs -------------------------------
  Wh   <- numeric(h)   # stratum weights computed on the *original* OSB (x)
  AWh  <- numeric(h)   # adjusted weights (for tail coverage; used for totals except 'unif')
  Nh   <- numeric(h)   # stratum population sizes (N * AWh or N * Wh for 'unif')
  Vh   <- numeric(h)   # stratum variances under the assumed distribution
  whsh <- numeric(h)   # pure Neyman contribution Wh * Sh (reported, cost-free)
  nume <- numeric(h)   # cost-optimal allocation weight Wh * Sh / sqrt(cost)
  deno <- 0            # sum of pure Wh*Sh across strata (the Neyman cost)
  adeno <- 0           # sum of cost-adjusted allocation weights
  nh   <- numeric(h)   # sample allocation per stratum
  fh   <- numeric(h)   # sampling fraction per stratum
  
  # ----------------------- tail adjustment window for AWh --------------------
  # For one-sided families (e.g., Weibull, Gamma, Exp, Lnorm, etc.), the OSB
  # may cut off small/non-negligible tails. We create 'y' as an adjusted window
  # used ONLY for AWh and Nh so weights sum approx  1 without biasing Vh, Wh, etc.
  y <- x
  d <- y[length(y)] - y[1]
  if (d < 10)          { y[1] <- 0;           y[length(y)] <- y[length(y)] + 10     }
  else if (d < 100)    { y[1] <- 0;           y[length(y)] <- y[length(y)] + 100    }
  else if (d < 1000)   { y[1] <- 0;           y[length(y)] <- y[length(y)] + 1000   }
  else if (d < 10000)  { y[1] <- 0;           y[length(y)] <- y[length(y)] + 10000  }
  else                 { y[1] <- 0;           y[length(y)] <- y[length(y)] + 100000 }
  
  # ----------------------- distribution-specific blocks ----------------------
  # Pattern per stratum:
  #   1) compute adjusted weight AWh[i] on y (tail-adjusted window)
  #   2) set Nh[i] = N * AWh[i]
  #   3) compute wh, E[X], E[X^2] on original OSB (x); Vh = EX2/wh - (EX/wh)^2
  #   4) compute nume[i] = Wh[i] * sqrt(Vh[i]) * sqrt(ch[i]); accumulate deno
  
  if (distr == "weibull") {
    r     <- my_env$obj[["params"]]["shape"]
    theta <- my_env$obj[["params"]]["scale"]
    g     <- 0
    
    f  <- function(x) (r/theta) * (((x-g)/theta)^(r-1)) * exp(-(((x-g)/theta)^r))
    f1 <- function(x) x  * f(x)
    f2 <- function(x) x^2 * f(x)
    WhW   <- function(a,b) integrate(f,  a, b)$value
    ExW   <- function(a,b) integrate(f1, a, b)$value
    Ex2W  <- function(a,b) integrate(f2, a, b)$value
    
    for (i in 1:(length(x)-1)) {
      AWh[i] <- WhW(y[i], y[i+1]);   Nh[i] <- N * AWh[i]
      wh     <- WhW(x[i], x[i+1])
      ex     <- ExW(x[i], x[i+1])  / wh
      ex2    <- Ex2W(x[i], x[i+1]) / wh
      Wh[i]  <- wh
      Vh[i]  <- ex2 - ex^2
      whsh[i] <- Wh[i] * sqrt(Vh[i]); nume[i] <- whsh[i] / sqrt(ch[i]); deno <- deno + whsh[i]; adeno <- adeno + nume[i]
    }
  }
  
  if (distr == "gamma") {
    r     <- my_env$obj[["params"]]["shape"]
    rate  <- my_env$obj[["params"]]["rate"]
    theta <- 1 / rate
    g     <- 0
    
    dens <- function(x) (((x-g)^(r-1))/((theta^r)*gamma(r))) * exp(-((x-g)/theta))
    f1   <- function(x) x  * dens(x)
    f2   <- function(x) x^2 * dens(x)
    WhG   <- function(a,b) integrate(dens, a, b)$value
    ExG   <- function(a,b) integrate(f1,   a, b)$value
    Ex2G  <- function(a,b) integrate(f2,   a, b)$value
    
    for (i in 1:(length(x)-1)) {
      AWh[i] <- WhG(y[i], y[i+1]);   Nh[i] <- N * AWh[i]
      wh     <- WhG(x[i], x[i+1])
      ex     <- ExG(x[i], x[i+1])  / wh
      ex2    <- Ex2G(x[i], x[i+1]) / wh
      Wh[i]  <- wh
      Vh[i]  <- ex2 - ex^2
      whsh[i] <- Wh[i] * sqrt(Vh[i]); nume[i] <- whsh[i] / sqrt(ch[i]); deno <- deno + whsh[i]; adeno <- adeno + nume[i]
    }
  }
  
  if (distr == "exp") {
    lambda <- my_env$obj[["params"]]["rate"]
    
    f   <- function(x) lambda * exp(-lambda * x)
    f1  <- function(x) x  * f(x)
    f2  <- function(x) x^2 * f(x)
    WhE   <- function(a,b) integrate(f,  a, b)$value
    ExE   <- function(a,b) integrate(f1, a, b)$value
    Ex2E  <- function(a,b) integrate(f2, a, b)$value
    
    for (i in 1:(length(x)-1)) {
      AWh[i] <- WhE(y[i], y[i+1]);   Nh[i] <- N * AWh[i]
      wh     <- WhE(x[i], x[i+1])
      ex     <- ExE(x[i], x[i+1])  / wh
      ex2    <- Ex2E(x[i], x[i+1]) / wh
      Wh[i]  <- wh
      Vh[i]  <- ex2 - ex^2
      whsh[i] <- Wh[i] * sqrt(Vh[i]); nume[i] <- whsh[i] / sqrt(ch[i]); deno <- deno + whsh[i]; adeno <- adeno + nume[i]
    }
  }
  
  if (distr == "norm") {
    mu  <- my_env$obj[["params"]]["mean"]
    sig <- my_env$obj[["params"]]["sd"]
    
    # For normal (two-sided support), extend y symmetrically so adjusted weights sum to ~1
    if (d < 10)          { y[1] <- y[1]-10;       y[length(y)] <- y[length(y)] + 10       }
    else if (d < 100)    { y[1] <- y[1]-100;      y[length(y)] <- y[length(y)] + 100      }
    else if (d < 1000)   { y[1] <- y[1]-1000;     y[length(y)] <- y[length(y)] + 1000     }
    else if (d < 10000)  { y[1] <- y[1]-10000;    y[length(y)] <- y[length(y)] + 10000    }
    else if (d < 100000) { y[1] <- y[1]-100000;   y[length(y)] <- y[length(y)] + 100000   }
    else                 { y[1] <- y[1]-100000;   y[length(y)] <- y[length(y)] + 100000   }
    
    f   <- function(x) (1/(sig*sqrt(2*pi))) * exp(-0.5 * ((x - mu)/sig)^2)
    f1  <- function(x) x  * f(x)
    f2  <- function(x) x^2 * f(x)
    WhN   <- function(a,b) integrate(f,  a, b)$value
    ExN   <- function(a,b) integrate(f1, a, b)$value
    Ex2N  <- function(a,b) integrate(f2, a, b)$value
    
    for (i in 1:(length(x)-1)) {
      AWh[i] <- WhN(y[i], y[i+1]);   Nh[i] <- N * AWh[i]
      wh     <- WhN(x[i], x[i+1])
      ex     <- ExN(x[i], x[i+1])  / wh
      ex2    <- Ex2N(x[i], x[i+1]) / wh
      Wh[i]  <- wh
      Vh[i]  <- ex2 - ex^2
      whsh[i] <- Wh[i] * sqrt(Vh[i]); nume[i] <- whsh[i] / sqrt(ch[i]); deno <- deno + whsh[i]; adeno <- adeno + nume[i]
    }
  }
  
  if (distr == "lnorm") {
    mu  <- my_env$obj[["params"]]["meanlog"]
    sig <- my_env$obj[["params"]]["sdlog"]
    gam <- 0
    
    f   <- function(x) (1/((x-gam)*sig*sqrt(2*pi))) * exp(-0.5 * ((log(x-gam)-mu)/sig)^2)
    f1  <- function(x) x  * f(x)
    f2  <- function(x) x^2 * f(x)
    WhL   <- function(a,b) integrate(f,  a, b)$value
    ExL   <- function(a,b) integrate(f1, a, b)$value
    Ex2L  <- function(a,b) integrate(f2, a, b)$value
    
    for (i in 1:(length(x)-1)) {
      AWh[i] <- WhL(y[i], y[i+1]);   Nh[i] <- N * AWh[i]
      wh     <- WhL(x[i], x[i+1])
      ex     <- ExL(x[i], x[i+1])  / wh
      ex2    <- Ex2L(x[i], x[i+1]) / wh
      Wh[i]  <- wh
      Vh[i]  <- ex2 - ex^2
      whsh[i] <- Wh[i] * sqrt(Vh[i]); nume[i] <- whsh[i] / sqrt(ch[i]); deno <- deno + whsh[i]; adeno <- adeno + nume[i]
    }
  }
  
  if (distr == "cauchy") {
    mu  <- my_env$obj[["params"]]["location"]
    sig <- my_env$obj[["params"]]["scale"]
    
    # Cauchy has very heavy tails; extend y aggressively
    if (d < 10)          { y[1] <- y[1]-10000;    y[length(y)] <- y[length(y)] + 10000    }
    else if (d < 100)    { y[1] <- y[1]-10000;    y[length(y)] <- y[length(y)] + 10000    }
    else if (d < 1000)   { y[1] <- y[1]-100000;   y[length(y)] <- y[length(y)] + 100000   }
    else if (d < 10000)  { y[1] <- y[1]-100000;   y[length(y)] <- y[length(y)] + 100000   }
    else if (d < 100000) { y[1] <- y[1]-1000000;  y[length(y)] <- y[length(y)] + 1000000  }
    else                 { y[1] <- y[1]-1000000;  y[length(y)] <- y[length(y)] + 1000000  }
    
    f   <- function(x) 1 / ((pi * sig) * (1 + ((x - mu)/sig)^2))
    f1  <- function(x) x  * f(x)
    f2  <- function(x) x^2 * f(x)
    WhC   <- function(a,b) integrate(f,  a, b)$value
    ExC   <- function(a,b) integrate(f1, a, b)$value
    Ex2C  <- function(a,b) integrate(f2, a, b)$value
    
    for (i in 1:(length(x)-1)) {
      AWh[i] <- WhC(y[i], y[i+1]);   Nh[i] <- N * AWh[i]
      wh     <- WhC(x[i], x[i+1])
      ex     <- ExC(x[i], x[i+1])  / wh
      ex2    <- Ex2C(x[i], x[i+1]) / wh
      Wh[i]  <- wh
      Vh[i]  <- ex2 - ex^2
      whsh[i] <- Wh[i] * sqrt(Vh[i]); nume[i] <- whsh[i] / sqrt(ch[i]); deno <- deno + whsh[i]; adeno <- adeno + nume[i]
    }
  }
  
  if (distr == "unif") {
    minn <- my_env$obj[["params"]]["min"]
    maxx <- my_env$obj[["params"]]["max"]
    
    f  <- function(x) 1 / (maxx - minn)
    f1 <- function(x) x  * f(x)
    f2 <- function(x) x^2 * f(x)
    f  <- Vectorize(f, "x")  # keep exact behavior from original code
    
    WhU   <- function(a,b) integrate(f,  a, b)$value
    ExU   <- function(a,b) integrate(f1, a, b)$value
    Ex2U  <- function(a,b) integrate(f2, a, b)$value
    
    for (i in 1:(length(x)-1)) {
      # Uniform used original Wh (no tail adjustment), consistent with your total logic
      wh     <- WhU(x[i], x[i+1])
      Nh[i]  <- N * wh
      ex     <- ExU(x[i], x[i+1])  / wh
      ex2    <- Ex2U(x[i], x[i+1]) / wh
      Wh[i]  <- wh
      Vh[i]  <- ex2 - ex^2
      whsh[i] <- Wh[i] * sqrt(Vh[i]); nume[i] <- whsh[i] / sqrt(ch[i]); deno <- deno + whsh[i]; adeno <- adeno + nume[i]
    }
  }
  
  if (distr == "triangle") {
    a <- my_env$obj[["params"]]["min"]
    b <- my_env$obj[["params"]]["max"]
    m <- my_env$obj[["params"]]["mode"]
    
    # piecewise triangular pdf and moments
    f1  <- function(x) (2*(x-a)) / ((b-a)*(m-a))          # rising part
    f1x <- function(x) x  * f1(x)
    f1x2<- function(x) x^2 * f1(x)
    f2  <- function(x) (2*(b-x)) / ((b-a)*(b-m))          # falling part
    f2x <- function(x) x  * f2(x)
    f2x2<- function(x) x^2 * f2(x)
    
    WhT1   <- function(l,u) integrate(f1,  l, u)$value
    ExT1   <- function(l,u) integrate(f1x, l, u)$value
    Ex2T1  <- function(l,u) integrate(f1x2,l, u)$value
    WhT2   <- function(l,u) integrate(f2,  l, u)$value
    ExT2   <- function(l,u) integrate(f2x, l, u)$value
    Ex2T2  <- function(l,u) integrate(f2x2,l, u)$value
    
    # adjusted bounds should match [a, b] for weight normalization
    y[1] <- a; y[length(y)] <- b
    
    for (i in 1:(length(x)-1)) {
      if (x[i+1] <= m) {
        AWh[i] <- WhT1(y[i], y[i+1]);   Nh[i] <- N * AWh[i]
        wh  <- WhT1(x[i], x[i+1])
        ex  <- ExT1(x[i], x[i+1])  / wh
        ex2 <- Ex2T1(x[i], x[i+1]) / wh
      } else if (x[i] <= m) {
        AWh[i] <- WhT1(y[i], m) + WhT2(m, y[i+1]);   Nh[i] <- N * AWh[i]
        wh  <- WhT1(x[i], m) + WhT2(m, x[i+1])
        ex  <- (ExT1(x[i], m) + ExT2(m, x[i+1]))   / wh
        ex2 <- (Ex2T1(x[i], m) + Ex2T2(m, x[i+1])) / wh
      } else {
        AWh[i] <- WhT2(y[i], y[i+1]);   Nh[i] <- N * AWh[i]
        wh  <- WhT2(x[i], x[i+1])
        ex  <- ExT2(x[i], x[i+1])  / wh
        ex2 <- Ex2T2(x[i], x[i+1]) / wh
      }
      Wh[i]  <- wh
      Vh[i]  <- ex2 - ex^2
      whsh[i] <- Wh[i] * sqrt(Vh[i]); nume[i] <- whsh[i] / sqrt(ch[i]); deno <- deno + whsh[i]; adeno <- adeno + nume[i]
    }
  }
  
  if (distr == "rtriangle") {
    a <- my_env$obj[["params"]]["min"]
    b <- my_env$obj[["params"]]["max"]
    
    f   <- function(x) (2*(b-x))/((b-a)^2)
    f1  <- function(x) x  * f(x)
    f2  <- function(x) x^2 * f(x)
    WhRT   <- function(l,u) integrate(f,  l, u)$value
    ExRT   <- function(l,u) integrate(f1, l, u)$value
    Ex2RT  <- function(l,u) integrate(f2, l, u)$value
    
    y[1] <- a; y[length(y)] <- b
    
    for (i in 1:(length(x)-1)) {
      AWh[i] <- WhRT(y[i], y[i+1]);   Nh[i] <- N * AWh[i]
      wh  <- WhRT(x[i], x[i+1])
      ex  <- ExRT(x[i], x[i+1])  / wh
      ex2 <- Ex2RT(x[i], x[i+1]) / wh
      Wh[i]  <- wh
      Vh[i]  <- ex2 - ex^2
      whsh[i] <- Wh[i] * sqrt(Vh[i]); nume[i] <- whsh[i] / sqrt(ch[i]); deno <- deno + whsh[i]; adeno <- adeno + nume[i]
    }
  }
  
  if (distr == "pareto") {
    if (!requireNamespace("actuar", quietly = TRUE)) {
      stop("Package 'actuar' needed, please install it!", call. = FALSE)
    }
    a <- my_env$obj[["params"]]["shape"]
    s <- my_env$obj[["params"]]["scale"]
    
    f   <- function(x) (a * s^a) / (x + s)^(a + 1)
    f1  <- function(x) x  * f(x)
    f2  <- function(x) x^2 * f(x)
    WhP   <- function(l,u) integrate(f,  l, u)$value
    ExP   <- function(l,u) integrate(f1, l, u)$value
    Ex2P  <- function(l,u) integrate(f2, l, u)$value
    
    for (i in 1:(length(x)-1)) {
      AWh[i] <- WhP(y[i], y[i+1]);   Nh[i] <- N * AWh[i]
      wh  <- WhP(x[i], x[i+1])
      ex  <- ExP(x[i], x[i+1])  / wh
      ex2 <- Ex2P(x[i], x[i+1]) / wh
      Wh[i]  <- wh
      Vh[i]  <- ex2 - ex^2
      whsh[i] <- Wh[i] * sqrt(Vh[i]); nume[i] <- whsh[i] / sqrt(ch[i]); deno <- deno + whsh[i]; adeno <- adeno + nume[i]
    }
  }
  
  # ----------------------- Neyman allocation & oversample fix ----------------
  # nh proportional to Wh * Sh * sqrt(cost) with sum(nh) = n; then fix oversampling if any nh > Nh
  if (is.na(adeno) || adeno <= 0) adeno <- 1
  for (i in 1:(length(x) - 1)) nh[i] <- n * nume[i] / adeno
  
  realloc(h, x, nh, Nh, nume, my_env)
  nh <- my_env$nh
  
  # one more check, same as original logic
  for (i in 1:(length(x) - 1)) {
    if (nh[i] > Nh[i]) {
      realloc(h, x, nh, Nh, nume, my_env)
      nh <- my_env$nh
    } else {
      nh <- my_env$nh
    }
  }
  
  # ----------------------- assemble user-facing outputs ----------------------
  # v2.0-2: whole-number sample sizes that add up to n (largest remainder)
  nh <- .round_alloc(nh, round(Nh))
  fh <- round(nh / Nh, 2)
  my_env$out <- data.frame(nh = nh, Nh = round(Nh), fh = fh)
  
  # Build the summary table ONCE (AWh for all except uniform, which uses Wh)
  outWh <- if (distr == "unif") Wh else AWh
  my_env$output <- data.frame(
    Wh   = round(outWh, 2),
    Vh   = round(Vh,    2),
    WhSh = round(whsh,  3)
  )
  
  # ----------------------- totals for summary() ------------------------------
  my_env$deno  <- round(deno, 3)
  my_env$WhTot <- if (distr == "unif") round(sum(Wh), 2) else round(sum(AWh), 2)
  my_env$NhTot <- round(sum(Nh))
  my_env$nhTot <- round(sum(nh))
  my_env$VhTot <- round(sum(Vh), 2)
  my_env$fhTot <- round(my_env$nhTot / my_env$NhTot, 2)
}
######################################################################