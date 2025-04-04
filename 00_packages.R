################################################################################
# UFP/mortality project


# Packages

################################################################################

# Data prep/cleaning 
library(readxl) # for uploading UFP data
library(openair) # for aggregating UFP data

# General data management 
library(dplyr) 
library(data.table)
library(tidyr)
library(lubridate)

# Analysis
library(Epi)
library(dlnm)
library(splines)
library(tsModel)
library(mixmeta) # for pooling West Midlands sites

# Plotting
library(ggplot2)

