# Prepare LETRS data
  ## merge in district codes

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
  
  ## letrs cohort data
  letrs_cohort = fread(paste0(rawdatadir, 'letrs_cohorts.csv'))
  
# prep data ----

  ## create district file
  district = unique(rcd_location[agency_level == 'LEA', .(agency_code, name)])
  
  ## update letrs file to match names in district file
  letrs_cohort[district == 'Clinton Schools', district := 'Clinton City Schools']
  letrs_cohort[district == 'Durham County Schools', district := 'Durham Public Schools']
  letrs_cohort[district == 'Elizabeth City- Pasquotank Public Schools', district := 'Elizabeth City-Pasquotank Public Schools']
  letrs_cohort[district == 'Gaston County', district := 'Gaston County Schools']
  letrs_cohort[district == 'Kannapolis City', district := 'Kannapolis City Schools']
  letrs_cohort[district == 'Mooresville Graded Schools', district := 'Mooresville Graded School District']
  letrs_cohort[district == 'New Hanover Schools', district := 'New Hanover County Schools']
  
  ## compare district and letrs file
  district_only = setdiff(district[, name], letrs_cohort[, district])
  letrs_only = setdiff(letrs_cohort[, district], district[, name])
    
  print(district_only)
  print(letrs_only) ## only one district here, not represented in NC data
    
  ## merge in cohorts
  letrs = merge.data.table(district,
                           letrs_cohort,
                           by.x = 'name',
                           by.y = 'district',
                           all.x = TRUE)
  
  ## manually add in 298LEA: schools for deaf and blind (no district level record in rcd_location)
  letrs = rbind(letrs,
                data.table(name = 'NC Schools for the Deaf and Blind',
                           agency_code = '298LEA',
                           region = 'north central',
                           letrs_cohort = 3))
    
  ## make sure all districts that should have cohort assignment do have it
  letrs[, has_cohort := max(as.integer(!is.na(letrs_cohort))), by=c('agency_code')]
  letrs[has_cohort == 0] ## these four districts do not appear in list of LETRS cohorts
  
  ## convert district file to district level
  letrs = unique(letrs[!is.na(letrs_cohort), .(agency_code, region, letrs_cohort)])
  stopifnot(nrow(letrs) == nrow(unique(letrs[, .(agency_code)])) & nrow(letrs) == 116) ## should have 116 districts
  
  ### update names to match other data
  setnames(letrs, c('agency_code'), c('lea_code'))
  
# save data ----
fwrite(letrs, paste0(workdatadir, 'prep_letrs_cohorts.csv'))