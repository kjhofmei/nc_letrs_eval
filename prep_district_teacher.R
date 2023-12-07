# Prepare demographics data

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
    
  ## teacher counts
  rcd_eq = fread(paste0(workdatadir, 'rcd_eq.csv'))
    
# prep data ----
  
  ## filter to obs we need: charters and district totals
  rcd_eq = rcd_eq[grepl('[A-Z]', agency_code) & !grepl('^NC', agency_code)]
  rcd_eq = rcd_eq[!(grepl('LEA$', agency_code) & poverty != '')]
  
  ## update agency code for charters to match other files
  rcd_eq[grepl('[A-Z]', agency_code) & !grepl('LEA$', agency_code), agency_code := paste0('CH_', agency_code)]
  
  ## keep cols we can trust (for now, just pct new teachers)
  rcd_eq = rcd_eq[, .(year, agency_code, pct_beg_teachers)]
  
  ## convert pct to [0, 100]
  rcd_eq[, pct_beg_teachers := 100 * pct_beg_teachers]
  
  ## harmonize with other files
  setnames(rcd_eq, 'agency_code', 'lea_code')
  
# save data ----
fwrite(rcd_eq, paste0(workdatadir, 'prep_district_teacher.csv'))