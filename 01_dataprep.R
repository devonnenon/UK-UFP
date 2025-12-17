################################################################################
# Reproducible code for the analysis of non-accidental mortality in:
# 
#   Mortality risks associated with short-term exposure to ultrafine particles 
#   in London and the West Midlands
# 
#   Nenon D, Fuller G, Masselot P, Gasparrini, A.
#   Environmental Epidemiology - 2025
#
#
# Load and prep dataset
#
################################################################################

#---------------
# Load data
#---------------

# Load data
data <- fread("ufpmortality_data.csv") %>%
  mutate(dow = as.factor(dow),
         holiday = as.factor(holiday))

str(data)
summary(data)

# Seperate into list by measurement site
sites <- unique(data$site)

dlist <- lapply(unique(data$site), function(x){
  sitedata <- data %>%
    filter(site == x)
  return(as.data.frame(sitedata))
})
names(dlist) <- sites

# Create moving averages for UPF, NO2, and PM2.5
dlist <- lapply(dlist, function(data){
  data$ufp01 <- runMean(data$ufp, 0:1)
  data$no201 <- runMean(data$no2, 0:1)
  data$pm2501 <- runMean(data$pm25, 0:1)
  return(data)
})

#---------------
# Remove unneeded objects
#---------------
rm(data)
