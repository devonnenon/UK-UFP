################################################################################
# Code for the analysis in:
# 
#   Mortality risks associated with short-term exposure to ultrafine particles 
#   in London and the West Midlands
# 
#   Nenon D, Fuller G, Masselot P, Gasparrini, A.
#   Environmental Epidemiology - 2025
#
#
# Import packages
#
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
library(patchwork)


