################################################################################
# UFP/mortality project


# Importing and cleaning UFP data

# Instructions for downloading publicly available data:
#-	Data downloaded yearly from 2000-2011, then grouped 2012-2015 and 2016-2019 
#  (2000-2009 is available yearly, 2010-2019 can be downloaded in groups. 
#  Grouping indicated here is arbitrary; max 5 years can be downloaded at a time)
#-	2000-2009 data from https://uk-air.defra.gov.uk/data/particle-data 
#  (UK Particle Monitoring Programme). Found under Data by pollutant, CPC. 
#   Click to download excel files. Must download each year separately
#-	2010-2019 data from https://uk-air.defra.gov.uk/data/ (Data Archive).  
#  Data Tools -> Get measured data and simple statistics -> 
#  Search Hourly Networks -> Start Now -> Select Data Type: Measured Data (Save) 
#  -> Select Date Range: Custom Date (Save) -> Select Monitoring Sites: 
#  Monitoring Network: Particle Concentrations and Numbers Network: 
#  Select All (Save) -> Select Pollutants: Select Pollutants by: 
#  Pollutant Name: Total Number Concentration (CPC) (Save) -> 
#  Selected Output Type: Data to Email Address (CSV): type email address, 
#  check Agree Terms (Save) -> “Get Data” button on the right side of the screen 

################################################################################
library(readxl)
library(openair)
library(dplyr)
library(data.table)
library(tidyr)

# Create vector of dowloaded file names, with names indicating year
files <- c(
  ufp00 = "Defra Particles CPC 2000 Final.xls",
  ufp01 = "Defra Particles CPC 2001 Final.xls",
  ufp02 = "Defra Particles CPC 2002 Final.xls",
  ufp03 = "Defra Particles CPC 2003 Final.xls",
  ufp04 = "Defra Particles CPC 2004 Final.xls",
  ufp05 = "Defra Particle Network CPC 2005.xls",
  ufp06 = "CPC2006.xls",
  ufp07 = "Defra_Particle_Network_CPC_2007.xls",
  ufp08 = "Defra_Particle_Network_CPC_2008.xls",
  ufp09 = "Defra_Particle_Network_CPC_2009.xls",
  ufp10 = "AirQualityDataHourly2010.csv",
  ufp11 = "AirQualityDataHourly2011.csv",
  ufp12_15 = "AirQualityDataHourly2012_2015.csv",
  ufp16_19 = "AirQualityDataHourly2016_2019.csv"
  )

# Load the data from all files into a list
ufp_list <- lapply(files, function(file){
  # if file is xls, else if file is cvs (extracts the end of file name after .)
  if(sub('.*\\.', '', file) == "xls") {
    colheadings <- read_excel(paste0("UFPdata/",file), sheet = "Data", n_max = 1)
    coltypes <- c("date", rep("numeric", length(colheadings)-1))
    df <- read_excel(paste0("UFPdata/", file), sheet = "Data", .name_repair = "universal", 
                     col_types = coltypes)
  } else {
  # extract column names from row 4
   colnames <- read.csv(paste0("UFPdata/", file), skip = 3, nrows = 1, header = T)
   # get data, starting at row 11 to ignore heading rows
   df <- fread(paste0("UFPdata/", file), skip = 11, header = F)
   # add column names to the data
   names(df) <- names(colnames)
   return(df)
  }
})
names(ufp_list) <- names(files)

# Change the date in the last file to date, read in as string
ufp_list[["ufp16_19"]]$Site.Name <- as.Date(ufp_list[["ufp16_19"]]$Site.Name, "%m/%d/%Y")

# Delete extra rows that are hidden in 2005 file
ufp_list[["ufp05"]] <- ufp_list[["ufp05"]] %>%
  select(1:9)

# Fix shifted times in 2008-2009 and 2005
halfhour <- 30*60 # 30 minutes, 60 seconds
ufp_list[["ufp08"]]$Date.Time <- ufp_list[["ufp08"]]$Date.Time - halfhour
ufp_list[["ufp09"]]$Date.Time <- ufp_list[["ufp09"]]$Date.Time - halfhour
ufp_list[["ufp05"]][5:nrow(ufp_list[["ufp05"]])-1,]$Date.Time <- # Only some rows
  ufp_list[["ufp05"]][5:nrow(ufp_list[["ufp05"]])-1,]$Date.Time +1 # add one second

# Remove extra duplicate entry from end of 2004
ufp_list[["ufp04"]] <- ufp_list[["ufp04"]] %>%
  filter(row_number() <= n()-1)

# Extract data for selected sites from each file and combine into single data frame
ufp <- do.call(rbind, lapply(ufp_list, function(df){
  # First column always has date data, add it to a new column named date
  df$date <- as.Date(df%>%pull(1))
  # find the index of the column that has "Kensington" in its name
  nkensindex <- which(grepl("Kensington", names(df)))
  # if such a column exists, add that data to a new column nkens in numeric format, else NA
  if (length(nkensindex)>0) df$london <- as.numeric(df%>%pull(nkensindex)) else df$london <- as.numeric(NA)
  birmindex <- which(grepl("Birmingham", names(df)))
  if (length(birmindex)>0) df$wmid <- as.numeric(df%>%pull(birmindex)) else df$wmid <- as.numeric(NA)
  df <- df %>%
    select(date, london, wmid)
  return(df)
}))

# Remove all measurements below 1 (implausible values)
ufp <- ufp %>%
  mutate(london = ifelse(london < 1, NA, london)) %>%
  mutate(wmid = ifelse(wmid < 1, NA, wmid))

# Remove 2003-08-18 and 2003-08-17 due to indication of error (extreme high values before period of missings)
#ufp$wmid[ufp$date == as.Date("2003-08-18") | ufp$date == as.Date("2003-08-17") ] <- NA

# For sensitivity analysis: trim top and bottom 5% 
#ufp <- ufp %>%
#  mutate(london = ifelse(london <= quantile(london, 0.05, na.rm=T), NA, london)) %>%
#  mutate(wmid = ifelse(wmid <= quantile(wmid, 0.05, na.rm=T), NA, wmid)) %>%
#  mutate(london = ifelse(london >= quantile(london, 0.95, na.rm=T), NA, london)) %>%
#  mutate(wmid = ifelse(wmid >= quantile(wmid, 0.95, na.rm=T), NA, wmid))

# Aggregate to daily, requiring 75% of complete measurements per day
ufp_agg <- timeAverage(ufp, avg.time = "day", data.thresh = 75)

# Change date back to date format 
ufp_agg$date <- as.Date(ufp_agg$date)

# Change all of 2017 to NA (Repoerted issues with the equipment, strangely low values)
ufp_agg <- ufp_agg %>%
  mutate(london = ifelse(year(date) == 2017, NA, london)) %>%
  mutate(wmid = ifelse(year(date) == 2017, NA, wmid))

# Pivot to longer format and add site column
ufp_agg_long <- ufp_agg %>%
  pivot_longer(cols = c(london, wmid), cols_vary = "slowest", 
               names_to = "area", values_to = "ufp") %>%
  # add site column to indicate the site change in birmingham 
  mutate(site = ifelse(area == "wmid" & date < as.Date("2009-01-12"), "birmcen", "birmtyb")) %>%
  # add site column for london data
  mutate(site = ifelse(area == "london", "nkens", site)) %>%
  # reorder columns
  select(date, area, site, ufp)

  
# Save data
write.csv(ufp_agg_long, file = "UFPdata/UFP_cleaned.csv", row.names = F)


