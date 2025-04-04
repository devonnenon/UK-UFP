################################################################################
# UFP/mortality project


# Import and combine UFP/mortality/covariates data
# Define parameters

################################################################################

#---------------
# Load data
#---------------

# Load UFP data
ufp <- read.csv("UFPdata/UFP_cleaned.csv")

# Load mortality and covariates data, add area column
mortenv <- read.csv("data/LndWestMidBUA_mortality_env_2003_2019.csv") %>%
  mutate(area = ifelse(BUA11NM == "Greater London BUA", "london", "wmid"))

# Combine data sets, reorder columns
combinedata <- mortenv %>%
  left_join(ufp, join_by(date, area)) %>%
  select(date, area, site, ufp, nonext, cvd, resp, tmean, no2, pm25)

# Reformat date to date class
combinedata$date <- as.Date(combinedata$date)

# Add day of week column
combinedata$dow <- as.factor(format(combinedata$date, "%u"))

# Add public holidays data
# Read in holidays data, sourced from publicly available API, keep only England column
holidays <- readRDS("data/holidays_GB_1980_2025.RDS") %>%
  subset(GB_ENG == TRUE) %>%
  dplyr::select(-name, -GB_SCT, -GB_WLS, -GB_NIR) 

# Merge with data and create indicator column (1 for holiday, 0 for not)
combinedata <- combinedata %>%
  left_join(holidays, by = join_by("date")) %>%
  mutate(holiday = ifelse(is.na(GB_ENG), 0, 1)) %>%
  select(-GB_ENG) 

# Seperate into list, by measurement site
dlist <- lapply(unique(combinedata$site), function(x){
  sitecombinedata <- combinedata %>%
    filter(site == x)
  return(sitecombinedata)
})
names(dlist) <- unique(combinedata$site)

# Create UFP moving average
dlist <- lapply(dlist, function(data){
  data$ufp01 <- runMean(data$ufp, 0:1)
  return(data)
})

# Trim to exclude periods of missing data at beginning or end
dlist <- lapply(dlist, function(data){
  subrange <- range(seq(nrow(data))[!is.na(data$ufp)])
  data <- data[subrange[1]:subrange[2],]
  return(data)
})

#---------------
# Define parameters
#---------------

# MAIN ANALYSIS

# Spline of time
dfspltime <- 7 # per year
spltime_ex <- expression(ns(data$date, df=round(dfspltime*nrow(data)/365.25)))
    # Uses an expression that is evaluated for each site within the loop 

# Define the unit increase for calculating effect estimates
unitinc <- 10000

# Store the mortality outcomes included in the analysis 
outcomes <- c("nonext", "cvd", "resp")

# Define crossbasis for temperature
ktemp <- expression(quantile(data$tmean, c(10, 75, 90)/100, na.rm = T))
cbtemp_ex <- expression(crossbasis(data$tmean, lag = 3, argvar = list(fun = "ns", 
                    knots = eval(ktemp)), arglag = list(fun = "strata", breaks = 1)))

# Define vector of West Midlands sites for pooling
birmsites <- names(dlist)[2:3]

# SECONDARY ANALYSIS 

# Choose length of extended lag
lagufp <- 5 

# Define spline for UFP for nonlinear E-R
dfsplufp <- 3
kufp <- quantile(ufp$ufp, 0.5, na.rm=T)
splufp_ex <- expression(onebasis(data$ufp, "ns", knots = kufp))

# Define range for nonlinear predictions (to ignore outliers)
predup <- 50000 # upper limit for range of predictions (approx 99.5%)
preds <- seq(0, predup, length = 30)

# MISC

# Import QAIC function
QAIC <- function(model) {
  phi <- summary(model)$dispersion
  loglik <- sum(dpois(model$y, model$fitted.values, log=TRUE))
  return(-2*loglik + 2*summary(model)$df[3]*phi)
}

#---------------
# Remove unneeded objects
#---------------
rm(combinedata, holidays, mortenv)





