# Prepare school count data

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
  
  ## how many schools per district? 
    
    ### by type (elementary etc)
    district_counts_bytype = rcd_location[agency_level == 'SCH', .(school_n = .N), by=c('category_code', 'lea_code', 'year')]
    district_counts_bytype[category_code == 'E', category := 'elementary']
    district_counts_bytype[category_code == 'M', category := 'middle']
    district_counts_bytype[category_code == 'H', category := 'high']
    district_counts_bytype[category_code == 'A', category := 'elem/middle/high']
    district_counts_bytype[category_code == 'I', category := 'elem/middle']
    district_counts_bytype[category_code == 'T', category := 'middle/high']
    district_counts_bytype[, category_code := NULL]
    
    ### title I
    district_counts_title1 = rcd_location[agency_level == 'SCH', .(school_n = .N), by=c('title_i', 'lea_code', 'year')]
    district_counts_title1[title_i == 'Y', category := 'title i']
    district_counts_title1[, title_i := NULL]
    district_counts_title1 = district_counts_title1[category != '']
    
    ### all
    district_counts_all = rcd_location[agency_level == 'SCH', .(school_n = .N), by=c('lea_code', 'year')]
    district_counts_all[, category := 'all']
    
    ### combine
    district_counts = rbind(district_counts_bytype,
                           district_counts_title1,
                           district_counts_all)
    
# save data ----
fwrite(district_counts, paste0(workdatadir, 'prep_district_school_counts.csv'))