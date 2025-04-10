################################################################################
# UFP/mortality project


# Define parameters

################################################################################
# MAIN ANALYSIS

# Spline of time
dfspltime <- 7 # per year
spltimefun <- "ns"

# Define the unit increase for calculating effect estimates
unitinc <- 10000

# Store the sites and outcomes in vectors for consistency
outcomes <- c("nonext", "cvd", "resp")

birmsites <- c("birmcen", "birmtyb")

# Define param of temp function
arglagtmean <- list(fun="strata", breaks = 1) # passed to crossbasis for temp
lagtmean <- 3 # number of lagged days for temp crossbasis

# SECONDARY ANALYSIS 

# Choose length of extended lag
lagufp <- 5 
# Define parameters for ufp crossbasis
argvarufp <- list(fun = "lin")
arglagufp <- list(fun = "integer")

# Define spline for UFP for nonlinear E-R
kufp <- quantile(ufpdf$ufp, 0.5, na.rm=T) # 1 knot at 50%
bdkufp <- sapply(dlist, function(x) range(x$ufp, na.rm = T)) |> range() #boundary knots at range
splufpfun <- "ns"
splufp_param <- list(knots = kufp, Boundary.knots = bdkufp) # If using bs, add degree = 

# Define range for nonlinear predictions (to ignore outliers)
maxpred <- max(bdkufp) # upper limit for range of predictions
preds <- seq(0, maxpred, length = 50)

#--------------

# Import QAIC function
QAIC <- function(model) {
  phi <- summary(model)$dispersion
  loglik <- sum(dpois(model$y, model$fitted.values, log=TRUE))
  return(-2*loglik + 2*summary(model)$df[3]*phi)
}