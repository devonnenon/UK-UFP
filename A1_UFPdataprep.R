################################################################################
# IMPORTING AND CLEANING UFP DATA

#-	Data downloaded yearly from 2000-2011, then grouped 2012-2015 and 2016-2019 
#  (2000-2009 is available yearly, 2010-2019 can be downloaded in groups. 
#  Grouping indicated here is arbitrary. Max 5 years can be downloaded at a time)
#-	2000-2009 data from https://uk-air.defra.gov.uk/data/particle-data 
#  (UK Particle Monitoring Programme). Found under Data by pollutant, CPC. 
#  Must download each year separately
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
cpc_list <- lapply(files, function(file){
  # if file is xls, else if file is cvs (extracts the end of file name after .)
  if(sub('.*\\.', '', file) == "xls") {
    colheadings <- read_excel(paste0("UFPdata/",file), sheet = "Data", n_max = 1)
    coltypes <- c("date", rep("numeric", length(colheadings)-1))
    df <- read_excel(paste0("UFPdata/", file), sheet = "Data", .name_repair = "universal", 
                     col_types = coltypes)
  } else {
  # extract column names from row 4
   colnames <- read.csv(paste0("UFPdata/", file), skip = 3, nrows = 1, header = T)
   # get data, starting at row 11
   df <- fread(paste0("UFPdata/", file), skip = 11, header = F)
   # add column names to the data
   names(df) <- names(colnames)
   return(df)
  }
})
names(cpc_list) <- names(files)

# Change the date in the last file to date, read in as string
cpc_list[["ufp16_19"]]$Site.Name <- as.Date(cpc_list[["ufp16_19"]]$Site.Name, "%m/%d/%Y")

# Delete extra rows that are hidden in 2005 file
cpc_list[["ufp05"]] <- cpc_list[["ufp05"]] %>%
  select(1:9)

# Fix shifted times in 2008-2009
halfhour <- 30*60 # 30 minutes, 60 seconds
cpc_list[["ufp08"]]$Date.Time <- cpc_list[["ufp08"]]$Date.Time - halfhour
cpc_list[["ufp09"]]$Date.Time <- cpc_list[["ufp09"]]$Date.Time - halfhour

# Remove extra duplicate entry from end of 2004
cpc_list[["ufp04"]] <- cpc_list[["ufp04"]] %>%
  filter(row_number() <= n()-1)

# Extract data for selected sites from each file and combine into single data frame
cpc <- do.call(rbind, lapply(cpc_list, function(df){
  # First column always has date data, add it to a new column named date
  df$date <- as.Date(df%>%pull(1))
  # find the index of the column that has "Kensington" in its name
  nkensindex <- which(grepl("Kensington", names(df)))
  # if such a column exists, add that data to a new column nkens in numeric format, else NA
  if (length(nkensindex)>0) df$nkens <- as.numeric(df%>%pull(nkensindex)) else df$nkens <- as.numeric(NA)
  birmindex <- which(grepl("Birmingham", names(df)))
  if (length(birmindex)>0) df$birm <- as.numeric(df%>%pull(birmindex)) else df$birm <- as.numeric(NA)
  df <- df %>%
    select(date, nkens, birm)
  return(df)
}))

# Aggregate to daily, requiring 75% of complete measurements per day
cpc_agg <- timeAverage(cpc, avg.time = "day", data.thresh = 75)

# Seperate Birmingham data based on site change in 2009
cpc_agg <- cpc_agg %>%
  mutate(
    birm_centre = ifelse(date < as.Date("2009"), birm, NA),
    birm_tyburn = ifelse(date >= as.Date("2009"), birm, NA)
  )

