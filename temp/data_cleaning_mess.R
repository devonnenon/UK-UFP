################################################################################
df <- cpc_list[["ufp01"]]
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






# Select and rename the required columns 
test <- lapply(cpc, function(df){
  find <- grepl("Birmingham", names(df))
  index <- which(find)
  #df$nkens <- as.numeric(df%>%pull(index))
  return(index)
})

test <- lapply(cpc, function(df){
  birmindex <- which(grepl("Birmingan", names(df)))
  if (length(birmindex)>0) df$birm <- as.numeric(df%>%pull()) else df$birm <- NA
  return(birmindex)
})

test2 <- lapply(cpc, function(df){
  birmindex <- grepl("Birmingan", names(df))
  return(birmindex)
})

filepaths <- c(
  "UFPdata/Defra Particles CPC 2000 Final.xls",
  "UFPdata/Defra Particles CPC 2001 Final.xls",
  "UFPdata/Defra Particles CPC 2002 Final.xls",
  "UFPdata/Defra Particles CPC 2003 Final.xls",
  "UFPdata/Defra Particles CPC 2004 Final.xls",
  "UFPdata/Defra Particle Network CPC 2005.xls",
  "UFPdata/CPC2006.xls",
  "UFPdata/Defra_Particle_Network_CPC_2007.xls",
  "UFPdata/Defra_Particle_Network_CPC_2008.xls",
  "UFPdata/Defra_Particle_Network_CPC_2009.xls",
  "UFPdata/AirQualityDataHourly2010.csv",
  "UFPdata/AirQualityDataHourly2011.csv",
  "UFPdata/AirQualityDataHourly2012_2015.csv",
  "UFPdata/AirQualityDataHourly2016_2019.csv"
)

which(test[[14]])
df <- cpc[[1]]
find <- grepl("ensington", names(df))
index <- which(grepl("ensington", names(df)))
df$nkens <- as.numeric(df%>%pull(which(grepl("ensington", names(df)))))

df <- cpc[[6]]
find <- grepl("ensington", names(df))
index <- which(find)
df$nkens <- as.numeric(df%>%pull(index))


names(cpc[[6]])

df <- cpc[[14]]
test <- grepl("ensington", names(df))
test
which(test)
df[,7]

str(cpc)


test <- lapply(cpc, function(df){
  df[,colnames(df)[1]]
})

test <- lapply(cpc, function(df){
  df%>%pull(1)
})

df <- cpc[[1]]
df[,1]
test <- as.Date(df%>%pull(1))





# Create new column named date for each df using first column (which always contains the date)
cpc <- lapply(cpc, function(df){
  df$date <- as.Date(df%>%pull(1))
  return(df)
})

# Find columns with data for North Kenginston and Birmingham sites and add to new columns
cpc <- lapply(cpc, function(df){
  # find the index of the column that has "Kensington" in its name
  nkensindex <- which(grepl("Kensington", names(df)))
  # if such a column exists, add that data to a new column nkens in numeric format, else NA
  if (length(nkensindex)>0) df$nkens <- as.numeric(df%>%pull()) else df$nkens <- NA
  birmindex <- which(grepl("Birmingan", names(df)))
  if (length(birmindex)>0) df$birm <- as.numeric(df%>%pull()) else df$birm <- NA
  return(df)
})





cpc <- lapply(cpc, function(df){
  names(df[,1]) <- "date"
  return(df)
})
str(cpc)
cpc <- lapply(cpc, function(df){
  df <- df 
})
file <- "UFPdata/AirQualityDataHourly2016_2019.csv"
colnames <- read.csv(file, skip = 3, nrows = 1, header = T)
df <- fread(file, skip = 11, header = F, colClasses = list(Date=1))
names(df) <- names(colnames)
str(df)
return(df)

df <- fread("UFPdata/AirQualityDataHourly2012_2015.csv")

str(cpc[[1]])
class(cpc[[1]]$...1)

df <- cpc[[1]]
df2 <- cpc[[14]]

names(df)

str(df)
df$date <- as.Date(df$Site.Name)
str(df)
df[1,]
df[1]
df[,1]
