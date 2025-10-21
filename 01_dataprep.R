################################################################################
# UFP/mortality project


# Import and combine UFP/mortality/covariates data

################################################################################

#---------------
# Load data
#---------------

# Load UFP data
ufpdf <- fread("UFPdata/UFP_cleaned_allsites.csv") %>% 
  filter(site %in% c("kensington", "birmcen", "birmtyb"))

# Load mortality and covariates data, add area column
mortenv <- fread("data/LndWestMidBUA_mortality_env_2003_2019.csv") %>%
  mutate(area = ifelse(BUA11NM == "Greater London BUA", "london", "wmid")) %>%
  select(-BUA11NM)

# Run for kensington sensitivity analysis - replaces london data with kensington data
# mortenvkens <- fread("data/Kensington/Kens_mortality_env_2003_2019.csv") %>%
#   mutate(area = "london") %>%
#   select(-BUASD11NM, -lad11cdo)
# mortenv <- rbind(mortenv %>% filter(area == "wmid"), mortenvkens)

# Combine data sets, reorder columns
combinedata <- ufpdf[mortenv, on = .(date, area)]
setcolorder(combinedata, c("date", "area", "site"))

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
  return(as.data.frame(sitecombinedata))
})
names(dlist) <- unique(combinedata$site)

# Create moving averages for UPF, NO2, and PM2.5
dlist <- lapply(dlist, function(data){
  data$ufp01 <- runMean(data$ufp, 0:1)
  data$no201 <- runMean(data$no2, 0:1)
  data$pm2501 <- runMean(data$pm25, 0:1)
  return(data)
})

# Trim to exclude periods of missing data at beginning or end
dlist <- lapply(dlist, function(data){
  subrange <- range(seq(nrow(data))[!is.na(data$ufp)])
  data <- data[subrange[1]:subrange[2],]
  return(data)
})

#---------------
# Remove unneeded objects
#---------------
rm(combinedata, holidays, mortenv)
