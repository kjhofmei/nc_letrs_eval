# Prepare school descriptor data: 2019 only (treating this as baseline year for descriptives)

# prep environment ----

  ## get workspace ready
  rm(list = ls())
  setwd("~/Documents/Michigan/PUBPOL 712/rp/")

  ## set directories
  rawdatadir = "./raw_data/"
  workdatadir = "./working_data/"
  outdir = "./output/"
  codedir = "./code/"

  ## libraries
  library(readxl)
  library(data.table)
  library(lubridate)
  library(janitor)
  library(stringr)
  
# load csvs ----
  
  ## school-level information
  rcd_location = fread(paste0(workdatadir, 'rcd_location.csv'))
  
# prep data ----
  
  ## keep if in SY19 & school
  rcd_location = rcd_location[year == 2019 & agency_level == 'SCH']
  
  ## keep cols needed
  rcd_location = rcd_location[, .(agency_code, lea_code, category_code, residing_district)]
  
# save data ----
fwrite(rcd_location, paste0(workdatadir, 'prep_school_descriptors_19.csv'))