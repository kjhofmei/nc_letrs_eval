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
  
  ## average daily membership
  rcd_adm = fread(paste0(workdatadir, 'rcd_adm.csv'))
  
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
    
    ### harmonize
    setnames(district_counts, 'lea_code', 'agency_code')
    
  ## how many students?
    
    ### filter to district values only
    district_counts_student = rcd_adm[agency_code %like% 'LEA']
    district_counts_student[category_code == 'E', category := 'elementary']
    district_counts_student[category_code == 'M', category := 'middle']
    district_counts_student[category_code == 'H', category := 'high']
    district_counts_student[category_code == 'A', category := 'elem/middle/high']
    district_counts_student[category_code == 'I', category := 'elem/middle']
    district_counts_student[category_code == 'T', category := 'middle/high']
    district_counts_student[category_code == '', category := 'all']
    district_counts_student[, category_code := NULL]
    
    ### merge in school counts by type to go from average to total
    district_counts_student = merge.data.table(district_counts_student,
                                               district_counts,
                                               by=c('year', 'agency_code', 'category'),
                                               all.x = TRUE)
    
    ### fix errors
      
      ### mark missing
      district_counts_student[, missing_n := as.integer(category != 'all' & is.na(school_n))]
    
      ### 870LEA: if missing, set school to 1
      district_counts_student[category != 'all' & is.na(school_n) & agency_code == '870LEA', school_n := 1]
      
      ### 298LEA: school for blind/deaf, not included at this time
      
    ### totals
    district_counts_student[, student_num := avg_student_num * school_n]
    district_counts_student[category == 'all', student_num := avg_student_num]
      
    ### add missing counts to school count data
    district_counts_missing = district_counts_student[missing_n == 1 & !is.na(school_n), .(year, agency_code, category, school_n)]
    
    district_counts = rbind(district_counts,
                            district_counts_missing)
    
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
  
  ## merge district list with school counts
  district_counts = merge.data.table(district_counts,
                                     district,
                                     by=c('agency_code'),
                                     all.x = TRUE)
  district_counts = district_counts[!is.na(letrs_cohort)]
  
  ## merge district list with school counts
  district_counts_student = merge.data.table(district_counts_student,
                                             district,
                                             by=c('agency_code'),
                                             all.x = TRUE)
  district_counts_student = district_counts_student[!is.na(letrs_cohort)]
  
# save data ----
fwrite(district_assessment, paste0(workdatadir, 'district_assessment.csv'))
fwrite(district_counts, paste0(workdatadir, 'district_school_counts.csv'))
fwrite(district_counts_student, paste0(workdatadir, 'district_student_counts.csv'))

  
## district codes that do not match any LETRS district
### 269: fort liberty: admin by DOD
### 295: innovative
### 298: school for deaf
### 299: virtual
### 679: camp lejeune: admin by DOD
### 996: DOC
### 997: institution? special needs
### 998: prison