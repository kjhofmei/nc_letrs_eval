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
    
  ## district demographics
  district_demos = fread(paste0(rawdatadir, 'district_demos.csv'))

  ## charter demographics
  charter_demos = fread(paste0(rawdatadir, 'charter_demos.csv'))

  ## all EDS
  eds = fread(paste0(workdatadir, 'rcd_acc_eds.csv'))
    
# prep data ----
  
  ## harmonize district_demos
  
    ### create pct black, hispanic, white 
    district_demos[, black := 100 * (BLACKMale + BLACKFemale) / Total]
    district_demos[, hispanic := 100 * (HISPANICMale + HISPANICFemale) / Total]
    district_demos[, white := 100 * (WHITEMale + WHITEFemale) / Total]
    
    ### fix id
    district_demos[, LEA := as.character(LEA)]
    district_demos[str_length(LEA) == 2, LEA := paste0('0', LEA)]
    district_demos[, agency_code := paste0(LEA, 'LEA')]
    
    ### fix year
    setnames(district_demos, 'Year', 'year')
    
    ### go long
    district_demos = melt(district_demos, id.vars = c('agency_code', 'year'), measure.vars = c('black', 'white', 'hispanic'), variable.name = 'subgroup', value.name = 'pct')
  
  ## harmonize charter_demos
  
    ### create pct black, hispanic, white 
    charter_demos[, black := 100 * (BLACKMale + BLACKFemale) / Total]
    charter_demos[, hispanic := 100 * (HISPANICMale + HISPANICFemale) / Total]
    charter_demos[, white := 100 * (WHITEMale + WHITEFemale) / Total]
    
    ### fix id
    charter_demos[, agency_code := paste0('CH_', CS, '000')]
    
    ### fix year
    setnames(charter_demos, 'Year', 'year')
    
    ### go long
    charter_demos = melt(charter_demos, id.vars = c('agency_code', 'year'), measure.vars = c('black', 'white', 'hispanic'), variable.name = 'subgroup', value.name = 'pct')
  
  ## harmonize EDS
  
    ### keep LEAs & charters only
    eds = eds[grepl('[A-Z]', agency_code) & !grepl('^NC-', agency_code)]
    
    ### align cols
    eds = eds[, .(year, agency_code, pct= pct_eds, subgroup = 'EDS')]
    
    ## update charter agency code
    eds[grepl('[A-Z]', agency_code) & !grepl('LEA$', agency_code), agency_code := paste0('CH_', agency_code)]
    
  ## combine charter & district demos
  demos = rbind(charter_demos,
                district_demos,
                eds)
  
  ## harmonize with other files
  setnames(demos, 'agency_code', 'lea_code')
  
# save data ----
fwrite(demos, paste0(workdatadir, 'prep_district_demographics.csv'))