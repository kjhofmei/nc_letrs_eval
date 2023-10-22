########################
# Prep data            #
########################

# Prepare data for analysis

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
  
  ## performance grades
  
    ## 2022-23
    spg_23 = fread(paste0(workdatadir, 'spg_23.csv'))
    
  ## school assessments
  sa_23 = fread(paste0(workdatadir, 'school_assessment_23.csv'))
  sa_22 = fread(paste0(workdatadir, 'school_assessment_22.csv'))
  sa_21 = fread(paste0(workdatadir, 'school_assessment_21.csv'))
  sa_21_o = fread(paste0(workdatadir, 'school_assessment_21_o.csv'))
  sa_19 = fread(paste0(workdatadir, 'school_assessment_19.csv'))
  sa_18 = fread(paste0(workdatadir, 'school_assessment_18.csv'))
  
# prep data ----
    
  ## create district file
  district = unique(rcd_location[agency_level == 'LEA', .(agency_code, name)])
  
  ## merge in LETRS cohort data
  
    ### update letrs file to match names in district file
    letrs_cohort[district == 'Clinton Schools', district := 'Clinton City Schools']
    letrs_cohort[district == 'Durham County Schools', district := 'Durham Public Schools']
    letrs_cohort[district == 'Elizabeth City- Pasquotank Public Schools', district := 'Elizabeth City-Pasquotank Public Schools']
    letrs_cohort[district == 'Gaston County', district := 'Gaston County Schools']
    letrs_cohort[district == 'Kannapolis City', district := 'Kannapolis City Schools']
    letrs_cohort[district == 'Mooresville Graded Schools', district := 'Mooresville Graded School District']
    letrs_cohort[district == 'New Hanover Schools', district := 'New Hanover County Schools']
    
    district_only = setdiff(district[, name], letrs_cohort[, district])
    letrs_only = setdiff(letrs_cohort[, district], district[, name])
    
    print(district_only)
    print(letrs_only) ## only one district here, not represented in NC data
    
    ### merge in cohorts
    district = merge.data.table(district,
                                letrs_cohort,
                                by.x = 'name',
                                by.y = 'district',
                                all.x = TRUE)
    
    ### make sure all districts that should have cohort assignment do have it
    district[, has_cohort := max(as.integer(!is.na(letrs_cohort))), by=c('agency_code')]
    district[has_cohort == 0] ## these four districts do not appear in list of LETRS cohorts
    
  ## convert district file to district level
  district = unique(district[!is.na(letrs_cohort), .(agency_code, region, letrs_cohort)])
  stopifnot(nrow(district) == nrow(unique(district[, .(agency_code)])) & nrow(district) == 115) ## should have 115 districts
  
  ## prep school assessment data (district level)
  
    ### SY21 data: merge in den field from other original file
    sa_21[, den := NULL]
    nrow(sa_21)
    sa_21 = merge.data.table(sa_21,
                             sa_21_o[, .(lea_code, school_code, subject, subgroup, den)],
                             by=c('lea_code', 'school_code', 'subject', 'subgroup'),
                             all.x = TRUE)
    nrow(sa_21)
    
    ### combine all years' data
    district_assessment = rbind(sa_18[lea_code == school_code],
                                sa_19[lea_code == school_code],
                                sa_21[lea_code == school_code],
                                sa_22[lea_code == school_code],
                                sa_23[lea_code == school_code],
                                fill = TRUE)
    
    ### create agency_code to match other data
    district_assessment[, agency_code := paste0(lea_code, 'LEA')]
  
  ## merge district list with assessment file
  district_assessment = merge.data.table(district_assessment,
                                         district,
                                         by=c('agency_code'),
                                         all.x = TRUE)
  
# save data ----
fwrite(district_assessment, paste0(workdatadir, 'district_assessment.csv'))
  
  
  
  test = district_assessment[!is.na(letrs_cohort), (mean = weighted.mean(as.numeric(glp_pct), as.numeric(den), na.rm = TRUE)), by=c('letrs_cohort', 'reporting_year', 'subject')]
  test[, letrs_cohort := factor(letrs_cohort)]
  
  test_2020 = unique(test[, .(letrs_cohort, subject, reporting_year = 2020, V1 = NA)])
  
  test = rbind(test,
               test_2020)
  
p=  ggplot(test[subject=='RD03'], aes(x = reporting_year, y = V1, color = letrs_cohort)) +
    geom_line() + geom_point() +   theme_test()

p=  ggplot(test[subject=='EOG'], aes(x = reporting_year, y = V1, color = letrs_cohort)) +
  geom_line() + geom_point() +   theme_test()