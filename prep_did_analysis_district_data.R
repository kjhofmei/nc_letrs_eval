# Prepare data for did analysis

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

  ## letrs cohort data
  letrs_cohorts = fread(paste0(workdatadir, 'prep_letrs_cohorts.csv'))

  ## district assessments
  district_assessment = fread(paste0(workdatadir, 'prep_district_assessment.csv'))
  
  ## school assessment - for charters
  charter_assessment = fread(paste0(workdatadir, 'prep_school_assessment.csv'))
  
# prep data ----
  
  ## update district cols to match charter cols
  district_assessment[, glp_pct_min_imp := glp_pct]
  district_assessment[, glp_pct_max_imp := glp_pct]
  district_assessment[, glp_pct := NULL]
  
  ## update charter cols to match district cols
  charter_assessment[, lea_code := paste(lea_code, school_code, sep = '_')]
  charter_assessment[, school_code := NULL]
  
  ## add charter data to district assessment
  district_assessment = rbind(district_assessment[!(lea_code %in% c('CH', 'NC-SEA'))],
                              charter_assessment[grepl('CH', lea_code)])

  ## join assessment data, letrs cohorts
  district_assessment = merge.data.table(district_assessment,
                                       letrs_cohorts,
                                       by=c('lea_code'),
                                       all.x = TRUE)
  district_assessment[is.na(letrs_cohort), letrs_cohort := 0]
  
  ## create first treated variable
  district_assessment[, first_treat := 0]
  district_assessment[letrs_cohort %in% c(1, 2), first_treat := 2022]
  district_assessment[letrs_cohort %in% c(3), first_treat := 2023]
  
  ## create exclude indicator (lab schools for now)
  district_assessment[, exclude := as.integer(grepl('Z', lea_code))]
  
  ## convert school_code to numeric
  districts = unique(district_assessment[, .(lea_code)])
  setorder(districts, lea_code)
  districts[, lea_code_num := sequence(.N)]
  
  nrow(district_assessment)
  district_assessment = merge.data.table(district_assessment,
                                         districts,
                                         by=c('lea_code'),
                                         all.x = TRUE)
  nrow(district_assessment)
  
# save data ----
fwrite(district_assessment, paste0(workdatadir, 'prep_did_analysis_district_data.csv'))