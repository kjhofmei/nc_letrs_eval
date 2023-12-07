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

  ## school assessments
  school_assessment = fread(paste0(workdatadir, 'prep_school_assessment.csv'))
  
# prep data ----

  ## join assessment data, letrs cohorts
  school_assessment = merge.data.table(school_assessment,
                                       letrs_cohorts,
                                       by=c('lea_code'),
                                       all.x = TRUE)
  school_assessment[is.na(letrs_cohort), letrs_cohort := 0]
  
  ## create first treated variable
  school_assessment[, first_treat := 0]
  school_assessment[letrs_cohort %in% c(1, 2), first_treat := 2022]
  school_assessment[letrs_cohort %in% c(3), first_treat := 2023]
  
  ## create exclude indicator (lab schools for now)
  school_assessment[, exclude := as.integer(grepl('Z', lea_code) | grepl('Z', school_code))]
  
  ## convert school_code to numeric
  schools = unique(school_assessment[, .(school_code)])
  setorder(schools, school_code)
  schools[, school_code_num := sequence(.N)]
  
  nrow(school_assessment)
  school_assessment = merge.data.table(school_assessment,
                                       schools,
                                       by=c('school_code'),
                                       all.x = TRUE)
  nrow(school_assessment)
  
  ## variance clustering col
  school_assessment[lea_code != 'CH', cluster := lea_code]
  school_assessment[lea_code == 'CH', cluster := paste(lea_code, school_code, sep = '_')]
  
# save data ----
fwrite(school_assessment, paste0(workdatadir, 'prep_did_analysis_data.csv'))