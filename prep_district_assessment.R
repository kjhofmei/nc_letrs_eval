# Prepare district assessment data
  ## treat all charters as single district

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
    
  ## performance grades
  
    ## 2022-23
    spg_23 = fread(paste0(workdatadir, 'spg_23.csv'))
    
  ## school assessment
  sa_23 = fread(paste0(workdatadir, 'school_assessment_23.csv'))
  sa_22 = fread(paste0(workdatadir, 'school_assessment_22.csv'))
  sa_21 = fread(paste0(workdatadir, 'school_assessment_21.csv'))
  sa_21_o = fread(paste0(workdatadir, 'school_assessment_21_o.csv'))
  sa_19 = fread(paste0(workdatadir, 'school_assessment_19.csv'))
  sa_18 = fread(paste0(workdatadir, 'school_assessment_18.csv'))
  
# prep data ----
  
  ## SY21 data: merge in den field from other original file
  sa_21[, den := NULL]
  nrow(sa_21)
  sa_21 = merge.data.table(sa_21,
                           sa_21_o[, .(lea_code, school_code, subject, subgroup, den)],
                           by=c('lea_code', 'school_code', 'subject', 'subgroup'),
                           all.x = TRUE)
  nrow(sa_21)
  
  ## prep school assessment data (non-charter district level)
    
    ### combine all years' data
    district_assessment = rbind(sa_18[lea_code == school_code],
                                sa_19[lea_code == school_code],
                                sa_21[lea_code == school_code],
                                sa_22[lea_code == school_code],
                                sa_23[lea_code == school_code],
                                fill = TRUE)
    
    ### reformat lea_code to match other data
    district_assessment[, lea_code := paste0(lea_code, 'LEA')]
    district_assessment[grepl('NC', lea_code), lea_code := 'NC-SEA']
    
      ## no suppression/masking at district level for glp, a few obs/tests suppressed for ccr
      nrow(district_assessment[is.na(glp_pct) | glp_pct == '' | is.na(as.numeric(glp_pct))])
      nrow(district_assessment[is.na(ccr_pct) | ccr_pct == '' | is.na(as.numeric(ccr_pct))])
      
      ## use glp as main outcome
      
  ## prep charter school assessment data (aggregate up from school level)
    
    ### combine all years' data
    charter_assessment = rbind(sa_18[lea_name == 'Charter Schools'],
                               sa_19[lea_name == 'Charter Schools'],
                               sa_21[lea_name == 'Charter Schools'],
                               sa_22[lea_name == 'Charter Schools'],
                               sa_23[lea_name == 'Charter Schools'],
                               fill = TRUE)
    
    ### impute censored glp_pct values
    
      ### create numeric versions of pct cols
      pct_cols = names(charter_assessment)[names(charter_assessment) %like% 'pct']
      for(c in pct_cols){
        charter_assessment[, paste0(c, '_num') := as.numeric(get(c))]
      }
    
      ### inferred from available data
      charter_assessment[!is.na(as.numeric(glp_pct)), glp_pct_inf := as.numeric(glp_pct)]
      
      ### if lev3 - lev5 pct not missing, replace
      charter_assessment[is.na(glp_pct_inf) & !is.na(lev3_pct_num) & !is.na(lev4_pct_num) & !is.na(lev5_pct_num), glp_pct_inf := lev3_pct_num + lev4_pct_num + lev5_pct_num]
    
      ### if lev4, lev5 sum to 1 (or more), replace with 1
      charter_assessment[is.na(glp_pct_inf) & lev4_pct_num + lev5_pct_num >= 100, glp_pct_inf := 100]
      
      ### if lev1, lev2 nonmissing, replace with 1 - ...
      charter_assessment[is.na(glp_pct_inf) & !is.na(lev1_pct_num) & !is.na(lev2_pct_num), glp_pct_inf := 100 - lev1_pct_num - lev2_pct_num]
      
      ### create min imputation
      charter_assessment[, glp_pct_min_imp := glp_pct_inf]
      charter_assessment[is.na(glp_pct_min_imp) & glp_pct == '>95' & lev4_pct_num + lev5_pct_num >= 95, glp_pct_min_imp := lev4_pct_num + lev5_pct_num]
      charter_assessment[is.na(glp_pct_min_imp) & glp_pct == '>95', glp_pct_min_imp := 95]
      charter_assessment[is.na(glp_pct_min_imp) & glp_pct == '<5', glp_pct_min_imp := 0]
      
      ### create max imputation
      charter_assessment[, glp_pct_max_imp := glp_pct_inf]
      charter_assessment[is.na(glp_pct_max_imp) & glp_pct == '>95', glp_pct_max_imp := 100]
      charter_assessment[is.na(glp_pct_max_imp) & glp_pct == '<5', glp_pct_max_imp := 5]
      
      ### calculate estimate of totals for each pct col
      pct_cols = names(charter_assessment)[names(charter_assessment) %in% c('glp_pct_min_imp', 'glp_pct_max_imp')]
      for(c in pct_cols){
        charter_assessment[, paste0(c, '_ct') := as.numeric(get(c)) * .01 * den]
      }
      
      ### sum denominators, counts over year/subject
      sum_cols = names(charter_assessment)[names(charter_assessment) %like% 'den|_ct']
      charter_assessment_sums = charter_assessment[, lapply(.SD, sum), .SDcols = sum_cols, by=c('reporting_year', 'subject')]
      
      ### imputation extremes result in maximum 0.07% difference in total count of passing students - take average and don't worry about this right now
      charter_assessment_sums[, glp_pct_mean_imp_ct := rowMeans(.SD), .SDcols = c('glp_pct_min_imp_ct', 'glp_pct_max_imp_ct')]
      
      ### calculate new glp pct
      charter_assessment_sums[, glp_pct := 100 * (glp_pct_mean_imp_ct / den)]
    
  ### combine district and charter assessment
  assessment = rbind(district_assessment,
                      charter_assessment_sums[, .(lea_code = 'CH', reporting_year, subject, den, glp_pct)],
                      fill = TRUE)
  
  ### update names to match other data
  setnames(assessment, c('reporting_year'), c('year'))
  
  ### keep only cols we need
  keep_cols = c('year', 'lea_code', 'subject', 'den', 'glp_pct')
  drop_cols = c('lea_name', 'school_code', 'school_name', 'grade_span', 'title_1', 'subgroup', 'total_pct', '...22')
  assessment = assessment[, ..keep_cols]
    
# save data ----
fwrite(assessment, paste0(workdatadir, 'prep_district_assessment.csv'))